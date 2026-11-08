// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._

import java.util.concurrent.CancellationException
import scala.collection.immutable.ArraySeq

class ReferenceIndexSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess           = FileAccess.disk
  implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

  private def withWorkspace(test: WorkspaceState.Parsed => Assertion): Assertion = {
    val workspace = TestWorkspace
      .genParsedOK(
        "Abstract Contract One() { fn run() -> () { let wanted = 1 let other = wanted } }",
        "Abstract Contract Two() { fn run() -> () { let wanted = 2 let other = wanted } }"
      )
      .sample
      .value
    try test(workspace)
    finally { TestWorkspace.delete(workspace); () }
  }

  "Reference index" should {
    "return exactly matching identifiers in original traversal order, including dependencies" in withWorkspace {
      workspace =>
        val expected = WorkspaceSearcher.collectAllIsParsed(workspace).flatMap {
          parsed =>
            parsed.astSoft.fetch().value.parts.flatMap {
              part =>
                part.toNode.walk.collect {
                  case Node(identifier: SoftAST.Identifier, _) if Set("wanted", "run")(identifier.code.text) =>
                    SourceLocation.NodeSoft(identifier, SourceLocation.CodeSoft(part, parsed))
                }
            }
        }
        val index = ReferenceIndex.build(workspace)
        index.candidates(Seq("run", "wanted", "run")) shouldBe expected
        index.candidates(Seq("missing")) shouldBe empty
        index.candidates(Seq.empty) shouldBe empty
        index.candidates(Seq("wanted")).map(_.ast.code.text).distinct shouldBe ArraySeq("wanted")
    }

    "index direct event arguments independently of their names and preserve candidate order" in {
      val workspace = TestWorkspace
        .genParsedOK(
          "Abstract Contract One() { event E(eventIndexField: U256) fn run(eventIndexAlias: U256, eventIndexField: U256) -> () { emit E(eventIndexAlias) emit E(eventIndexAlias + 1) emit E(eventIndexField) } }"
        )
        .sample
        .value
      try {
        val index     = ReferenceIndex.build(workspace)
        val arguments = index.candidates(Seq.empty, includeEventArguments = true)
        arguments.map(_.ast.code.text) shouldBe ArraySeq("eventIndexAlias", "eventIndexField")
        index.candidates(Seq.empty) shouldBe empty
        val named    = index.candidates(Seq("eventIndexField"))
        val combined = index.candidates(Seq("eventIndexField"), includeEventArguments = true)
        combined.size shouldBe (named.size + 1)
        combined.distinct shouldBe combined
        combined.map(_.ast.index.index) shouldBe combined.map(_.ast.index.index).sorted
      } finally { TestWorkspace.delete(workspace); () }
    }

    "keep old and edited snapshots separate, including deleted files" in withWorkspace {
      workspace =>
        val cache    = SearchCache(1)
        val oldIndex = cache.get(workspace).referenceIndex
        val edited   = workspace.copy(sourceCode = workspace.sourceCode.take(1))
        val newIndex = cache.get(edited).referenceIndex
        oldIndex.candidates(Seq("wanted")) should have size 4
        newIndex.candidates(Seq("wanted")) should have size 2
        newIndex.candidates(Seq("wanted")).map(_.source.parsed.fileURI).distinct shouldBe ArraySeq(workspace.sourceCode.head.fileURI)
        cache.get(workspace).referenceIndex.candidates(Seq("wanted")) should have size 4
    }

    "reuse a completed index for repeated queries" in withWorkspace {
      workspace =>
        val cache = new WorkspaceSearchCache(workspace)
        val first = cache.referenceIndex
        cache.referenceIndex should be theSameInstanceAs first
        first.candidates(Seq("wanted")) shouldBe cache.referenceIndex.candidates(Seq("wanted"))
    }

    "cancel construction cooperatively without publishing partial results" in withWorkspace {
      workspace =>
        var visited = 0
        intercept[CancellationException] {
          ReferenceIndex.build(workspace, () => { visited += 1; visited > 5 })
        }
        ReferenceIndex.build(workspace).candidates(Seq("wanted")) should have size 4
    }
  }

}
