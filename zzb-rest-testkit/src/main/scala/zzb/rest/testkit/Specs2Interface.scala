package zzb.rest.testkit

import org.specs2.execute.{ Failure, FailureException }
import org.specs2.specification.{ FragmentsBuilder, SpecificationStructure, Fragments, Step }

trait Specs2Interface extends TestFrameworkInterface with SpecificationStructure {

  def failTest(msg: String) = {
    val trace = new Exception().getStackTrace.toList
    val fixedTrace = trace.drop(trace.indexWhere(_.getClassName.startsWith("org.specs2")) - 1)
    throw new FailureException(Failure(msg, stackTrace = fixedTrace))
  }

  override def map(fs: ⇒ Fragments) = super.map(fs).add(Step(cleanUp()))
}

trait NoAutoHtmlLinkFragments extends FragmentsBuilder {
  override def stringToHtmlLinkFragments2(s: String): HtmlLinkFragments2 = super.stringToHtmlLinkFragments2(s)
  override def stringToHtmlLinkFragments(s: String): HtmlLinkFragments = super.stringToHtmlLinkFragments(s)
}