/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.formatting;

import static org.junit.Assert.assertEquals;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.formatting.INodeModelFormatter;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.resource.XtextResource;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLFormatterTest.AgentTest.class,
	SARLFormatterTest.BehaviorTest.class,
	SARLFormatterTest.SkillTest.class,
	SARLFormatterTest.XbaseTest.class,
	SARLFormatterTest.EventTest.class,
	SARLFormatterTest.CapacityTest.class,
})
@SuppressWarnings("all")
public class SARLFormatterTest {

	protected static void assertFormat(ParseHelper<XtendFile> parser, INodeModelFormatter formatter,
			CharSequence input, CharSequence expected) throws Exception {
		ICompositeNode node = ((XtextResource) parser.parse(input).eResource()).getParseResult().getRootNode();
		String actual = formatter.format(node, 0, input.length()).getFormattedText();
		assertEquals(expected.toString(), actual);
	}

	public static class AgentTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void agentDefinition() throws Exception {
			String source = "agent A1 { } agent A2 {}";
			String expected = multilineString(
					"agent A1 {",
					"}",
					"",
					"agent A2 {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void whitespacesBetweenAgents() throws Exception {
			String source = multilineString(
					"agent A1 { }",
					"",
					"",
					"",
					"agent A2 {}"
					);
			String expected = multilineString(
					"agent A1 {",
					"}",
					"",
					"agent A2 {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void actions() throws Exception {
			String source = multilineString(
					"agent A {",
					"  uses Cap1, Cap",
					"",
					"  on E [occurrence.name == \"Hello\"] {",
					"    println(\"event is hello\") ",
					"    action2",
					"",
					"",
					"    action2",
					"",
					"",
					"",
					"    action2         action2",
					"  }",
					"",
					"  def action2 {",
					"    println(\"action2\")",
					"    println(\"action2..\") println(\"action2..\")",
					"  }",
					"}"
					);
			String expected = multilineString(
					"agent A {",
					"	uses Cap1, Cap",
					"",
					"	on E [occurrence.name == \"Hello\"] {",
					"		println(\"event is hello\")",
					"		action2",
					"",
					"		action2",
					"",
					"		action2 action2",
					"	}",
					"",
					"	def action2 {",
					"		println(\"action2\")",
					"		println(\"action2..\") println(\"action2..\")",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void testAttributes() throws Exception {
			String source = "agent A1 { var cmdAddr : Address var cmdAddr2 : Address }";
			String expected = multilineString(
					"agent A1 {",
					"	var cmdAddr : Address",
					"	var cmdAddr2 : Address",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void variableDeclaration() throws Exception {
			String source = multilineString(
					"agent A1 { var cmdAddr : Address var cmdAddr2 : Address",
					"  on E {",
					"  val cmd :Address",
					"  val cmd2 : Address",
					"",
					"",
					"}}"
					);
			String expected = multilineString(
					"agent A1 {",
					"	var cmdAddr : Address",
					"	var cmdAddr2 : Address",
					"",
					"	on E {",
					"		val cmd : Address",
					"		val cmd2 : Address",
					"",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void variableAssignment() throws Exception {
			String source = multilineString(
					"agent A1 { var cmdAddr : Address var cmdAddr2 : Address",
					"  on E {",
					"  val cmd :Address",
					"  val cmd2 : Address",
					"  cmd = null ",
					"  cmd2 = null",
					"}}"
					);
			String expected = multilineString(
					"agent A1 {",
					"	var cmdAddr : Address",
					"	var cmdAddr2 : Address",
					"",
					"	on E {",
					"		val cmd : Address",
					"		val cmd2 : Address",
					"		cmd = null",
					"		cmd2 = null",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void uses() throws Exception {
			String source = "agent A1 { uses Cap1 , Cap2, Cap3 }";
			String expected = multilineString(
					"agent A1 {",
					"	uses Cap1, Cap2, Cap3",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void behaviorUnit() throws Exception {
			String source = multilineString(
					"agent A1 { uses Cap1",
					"  on Initialize {} }"
					);
			String expected = multilineString(
					"agent A1 {",
					"	uses Cap1",
					"",
					"	on Initialize {",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void initializeBehaviorUnit() throws Exception {
			String source = multilineString(
					"agent A {",
					"  on Initialize { if(true) {println(\"hello\")} }",
					"}"
					);
			String expected = multilineString(
					"agent A {",
					"",
					"	on Initialize {",
					"		if(true) {",
					"			println(\"hello\")",
					"		}",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void initializeBehaviorUnit_noBrackets() throws Exception {
			String source = multilineString(
					"agent A {",
					"  on Initialize { if(true) println(\"hello\") else println(\"goodbye\") }",
					"}"
					);
			String expected = multilineString(
					"agent A {",
					"",
					"	on Initialize {",
					"		if(true) println(\"hello\") else println(\"goodbye\")",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

	public static class BehaviorTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void behaviorDefinition_0() throws Exception {
			String source = "behavior B1 { } behavior B2 {}";
			String expected = multilineString(
					"behavior B1 {",
					"}",
					"",
					"behavior B2 {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void behaviorsDefinition_1() throws Exception {
			String source = multilineString(
					"behavior B1 { ",
					"  requires Cap1, Cap2",
					"  uses Cap1",
					"  uses Cap2",
					"",
					"  def a1 {",
					"    //something",
					"  }",
					"",
					"  on E [] {",
					"      if(true == true) { println(\"Nice\")}",
					"  }",
					"}",
					"behavior B2 {}"
					);
			String expected = multilineString(
					"behavior B1 {",
					"	requires Cap1, Cap2",
					"	uses Cap1",
					"	uses Cap2",
					"",
					"	def a1 {",
					"	//something",
					"",
					"	}",
					"",
					"	on E [] {",
					"		if(true == true) {",
					"			println(\"Nice\")",
					"		}",
					"	}",
					"",
					"}",
					"",
					"behavior B2 {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

	public static class SkillTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void skillDeclaration() throws Exception {
			String source = multilineString(
					"skill S implements C{",
					"  new(ag:Agent){super(ag)}",
					"  def action(a:String){",
					"    println(a)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"skill S implements C {",
					"",
					"	new(ag : Agent) {",
					"		super(ag)",
					"	}",
					"",
					"	def action(a : String) {",
					"		println(a)",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

	public static class XbaseTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void base() throws Exception {
			String source = multilineString(
					"agent A {",
					"  on Initialize {",
					"    if(!dir.exists){",
					"      throw new IllegalArgumentException(\"Path [\"+myPath+\"] is not a Directory\")",
					"    }",
					"",
					"    for(f : dir.listFiles()){",
					"      if(f.directory){",
					"        if(!f.name.equals(\".\")&& !f.name.equals(\"..\")) {",
					"          val targetID = FileSearchAgent.spawnInContext(innerContext,#[f.absolutePath,false.toString]);",
					"          innerContext.defaultSpace.emit(new Search =>[source=innerContext.defaultSpace.getAddress(ID); date=maxDate;fileMatch = fMatch], Scopes.addresses(innerContext.defaultSpace.getAddress(targetID)));",
					"        }",
					"      }else{",
					"        if((maxDate.time >= f.lastModified) && !(f.name.startsWith(\".\")) && (f.name.endsWith(fMatch))){",
					"          emit(new FoundFile =>[file = f.absolutePath],Scopes.addresses(cmdAddr))",
					"        }",
					"      }",
					"    }",
					"    if (true == true){prinln (\"cool!\")}else{}",
					"",
					"  }",
					"}"
					);
			String expected = multilineString(
					"agent A {",
					"",
					"	on Initialize {",
					"		if(!dir.exists) {",
					"			throw new IllegalArgumentException(\"Path [\" + myPath + \"] is not a Directory\")",
					"		}",
					"",
					"		for ( f : dir.listFiles() ) {",
					"			if(f.directory) {",
					"				if(!f.name.equals(\".\") && !f.name.equals(\"..\")) {",
					"					val targetID = FileSearchAgent.spawnInContext(innerContext, # [ f.absolutePath , false.toString ]);",
					"					innerContext.defaultSpace.emit(new Search => [source = innerContext.defaultSpace.getAddress(ID) ; date = maxDate ; fileMatch = fMatch], Scopes.addresses(innerContext.defaultSpace.getAddress(targetID)));",
					"				}",
					"			} else {",
					"				if((maxDate.time >= f.lastModified) && !(f.name.startsWith(\".\")) && (f.name.endsWith(fMatch))) {",
					"					emit(new FoundFile => [file = f.absolutePath], Scopes.addresses(cmdAddr))",
					"				}",
					"			}",
					"		}",
					"		if(true == true) {",
					"			prinln(\"cool!\")",
					"		} else {",
					"		}",
					"",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void testImports() throws Exception {
			String source = multilineString(
					"import io.sarl.core.Agent import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import static java.lang.String . *",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import static java.lang.String.*",
					"import io.sarl.core.Agent import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent ",
					"import io.sarl.core.Event",
					"",
					"agent A {}"
					);
			String expected = multilineString(
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import static java.lang.String.*",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import static java.lang.String.*",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"import io.sarl.core.Agent",
					"import io.sarl.core.Event",
					"",
					"agent A {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void tryCatch() throws Exception {
			String source = multilineString(
					"agent A{",
					"def asInteger(s : String) : Integer {",
					"  var res : Integer = null",
					"",
					"      try      {",
					"        res = Integer.parseInt(s)",
					"      }    catch(NumberFormatException nfe){",
					"      } ",
					"",
					"return res",
					"}",
					"",
					"}"
					);
			String expected = multilineString(
					"agent A {",
					"",
					"	def asInteger(s : String) : Integer {",
					"		var res : Integer = null",
					"",
					"		try",
					"		{",
					"			res = Integer.parseInt(s)",
					"		}",
					"		catch(NumberFormatException nfe)",
					"		{",
					"		}",
					"",
					"		return res",
					"	}",
					"",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class EventTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void declaration() throws Exception {
			String source = "event E1 {} event E2 {}";
			String expected = multilineString(
					"event E1 {",
					"}",
					"",
					"event E2 {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void eventAndAgent() throws Exception {
			String source = "event E1 {} agent A {}";
			String expected = multilineString(
					"event E1 {",
					"}",
					"",
					"agent A {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void eventsAndAgentAndImport() throws Exception {
			String source = multilineString(
					"import io.sarl.core.Schedules",
					"",
					"event E1 {} agent A {}"
					);
			String expected = multilineString(
					"import io.sarl.core.Schedules",
					"",
					"event E1 {",
					"}",
					"",
					"agent A {",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

		@Test
		public void actions() throws Exception {
			String source = multilineString(
					"import io.sarl.core.Schedules",
					"",
					"event E1 {} agent A {",
					"",
					"  on Destroy {",
					"    println(\"I'm about to die, bye :-)\")",
					"  } def isInternal ( e : Event ) : boolean {",
					"    return e.source.spaceId.equals(innerContext.defaultSpace.ID);",
					"  } def hasMembers : boolean {",
					"    return innerContext.defaultSpace.participants.size > 1",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.core.Schedules",
					"",
					"event E1 {",
					"}",
					"",
					"agent A {",
					"",
					"	on Destroy {",
					"		println(\"I'm about to die, bye :-)\")",
					"	}",
					"",
					"	def isInternal(e : Event) : boolean {",
					"		return e.source.spaceId.equals(innerContext.defaultSpace.ID);",
					"	}",
					"",
					"	def hasMembers : boolean {",
					"		return innerContext.defaultSpace.participants.size > 1",
					"	}",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

	public static class CapacityTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<XtendFile> parser;

		@Inject
		private INodeModelFormatter formatter;

		@Test
		public void capacity() throws Exception {
			String source = multilineString(
					"capacity ExternalContextAccess {",
					"  /**",
					"   * Replies all contexts this agent is a member of, including the default context",
					"   */   ",
					"  def getAllContexts : Collection<AgentContext>",
					"",
					"/**",
					" * Replies the AgentContext for the given ID.",
					" * The agent must have joined the context before calling this action or use its parentContextID",
					" * @see{Agent#getParentID}",
					" * @see{#join}",
					" * ",
					" * @throws UnknownContextException if the context specified context is not known by the agent.",
					" * @param contextID the ID of the context to get.",
					" */",
					"def getContext(contextID : UUID): AgentContext",
					"",
					"/**",
					" * Joins a new parent context (a new super holon).",
					" * This actions registers the agent in the default Space of the parent Context.",
					" * . ",
					" * @fires ContextJoined in its inner Context default space (Behaviors#wake).",
					" * @fires MemberJoined in its parent Context default Space ",
					" */",
					"def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined",
					"  ",
					"/**",
					" * Leaves the parent's context.",
					" * @fires ContextLeft in its inner Context default space (Behaviors#wake).",
					" * @fires MemberLeft in its parent Context default Space ",
					" */",
					"def leave(contextID : UUID) fires ContextLeft, MemberLeft",
					"  ",
					"}"
					);
			String expected = multilineString(
					"capacity ExternalContextAccess {",
					"/**",
					"   * Replies all contexts this agent is a member of, including the default context",
					"   */",
					"	def getAllContexts : Collection <AgentContext>",
					"",
					"	/**",
					" * Replies the AgentContext for the given ID.",
					" * The agent must have joined the context before calling this action or use its parentContextID",
					" * @see{Agent#getParentID}",
					" * @see{#join}",
					" * ",
					" * @throws UnknownContextException if the context specified context is not known by the agent.",
					" * @param contextID the ID of the context to get.",
					" */",
					"	def getContext(contextID : UUID) : AgentContext",
					"",
					"	/**",
					" * Joins a new parent context (a new super holon).",
					" * This actions registers the agent in the default Space of the parent Context.",
					" * . ",
					" * @fires ContextJoined in its inner Context default space (Behaviors#wake).",
					" * @fires MemberJoined in its parent Context default Space ",
					" */",
					"	def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined",
					"",
					"	/**",
					" * Leaves the parent's context.",
					" * @fires ContextLeft in its inner Context default space (Behaviors#wake).",
					" * @fires MemberLeft in its parent Context default Space ",
					" */",
					"	def leave(contextID : UUID) fires ContextLeft, MemberLeft",
					"}"
					);
			assertFormat(this.parser, this.formatter, source, expected);
		}

	}

}
