/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString2;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.IOException;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;
import com.google.inject.Injector;

import foo.ecore.SubEvent;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.appenders.SarlSkillSourceAppender;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorUnitBuilder;
import io.sarl.lang.codebuilder.builders.ISarlCapacityBuilder;
import io.sarl.lang.codebuilder.builders.ISarlSkillBuilder;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;


/** Testing class for issue: Cannot create Skill implementing a capacity with a generic type.
 *
 * <p>https://github.com/sarl/sarl/issues/1135
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/sarl/sarl/issues/1135"
 */
@DisplayName("Bug #1135")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class Bug1135Test {

	@Nested
	@DisplayName("Capacity created by builder")
	public class CapacityTest extends AbstractSarlTest { 

		@Inject
		private CodeBuilderFactory factory;
	
		private ResourceSet resource;
	
		private ISarlCapacityBuilder builder;
	
		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlCapacity("C", this.resource);
		}
	
		@Test
		@DisplayName("getData")
		public void getData() {
			var actionBuilder = this.builder.addDefSarlAction("getData");
			var typeParameterBuilder = actionBuilder.addTypeParameter("T");
			var typeParameter = typeParameterBuilder.getJvmTypeParameter();
			actionBuilder.addParameter("name").setParameterType("String");
			var typeParameterRef0 = typeParameterBuilder.newTypeRef(typeParameter);
			actionBuilder.addParameter("type").setParameterType(actionBuilder.newTypeRef(Class.class, typeParameterRef0));
			var typeParameterRef1 = typeParameterBuilder.newTypeRef(typeParameter);
			actionBuilder.setReturnType(typeParameterRef1);
			//
			var capacity = this.builder.getSarlCapacity();
			assertEquals(1, capacity.getMembers().size());
			var mbr = capacity.getMembers().get(0);
			var act = assertInstanceOf(SarlAction.class, mbr);
	
			assertEquals("getData", act.getName());
	
			assertEquals(1, act.getTypeParameters().size());
			var tp = assertInstanceOf(JvmTypeParameter.class, act.getTypeParameters().get(0));
			assertEquals("T", tp.getName());
			assertEquals(1, tp.getConstraints().size());
			var ub = assertInstanceOf(JvmUpperBound.class, tp.getConstraints().get(0));
			assertEquals(Object.class.getName(), ub.getTypeReference().getIdentifier());
			
			assertEquals(2, act.getParameters().size());
			var param0 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(0));
			assertEquals("name", param0.getName());
			assertEquals("java.lang.String", param0.getParameterType().getType().getIdentifier());
			var param1 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(1));
			assertEquals("type", param1.getName());
			assertEquals("java.lang.Class", param1.getParameterType().getType().getIdentifier());
	
			var rt = act.getReturnType();
			assertNotNull(rt);
			assertSame(tp, rt.getType());
		}
	
		@Test
		@DisplayName("setData")
		public void setData() {
			var actionBuilder = this.builder.addDefSarlAction("setData");
			var typeParameterBuilder = actionBuilder.addTypeParameter("T");
			var typeParameter = typeParameterBuilder.getJvmTypeParameter();
			actionBuilder.addParameter("name").setParameterType("String");
			var typeParameterRef0 = typeParameterBuilder.newTypeRef(typeParameter);
			actionBuilder.addParameter("type").setParameterType(actionBuilder.newTypeRef(Class.class, typeParameterRef0));
			var typeParameterRef1 = typeParameterBuilder.newTypeRef(typeParameter);
			actionBuilder.addParameter("value").setParameterType(typeParameterRef1);
			var typeParameterRef2 = typeParameterBuilder.newTypeRef(typeParameter);
			actionBuilder.setReturnType(typeParameterRef2);
			//
			var capacity = this.builder.getSarlCapacity();
			assertEquals(1, capacity.getMembers().size());
			var mbr = capacity.getMembers().get(0);
			var act = assertInstanceOf(SarlAction.class, mbr);
	
			assertEquals("setData", act.getName());
	
			assertEquals(1, act.getTypeParameters().size());
			var tp = assertInstanceOf(JvmTypeParameter.class, act.getTypeParameters().get(0));
			assertEquals("T", tp.getName());
			assertEquals(1, tp.getConstraints().size());
			var ub = assertInstanceOf(JvmUpperBound.class, tp.getConstraints().get(0));
			assertEquals(Object.class.getName(), ub.getTypeReference().getIdentifier());
			
			assertEquals(3, act.getParameters().size());
			var param0 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(0));
			assertEquals("name", param0.getName());
			assertEquals("java.lang.String", param0.getParameterType().getType().getIdentifier());
			var param1 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(1));
			assertEquals("type", param1.getName());
			assertEquals("java.lang.Class", param1.getParameterType().getType().getIdentifier());
			var param2 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(2));
			assertEquals("value", param2.getName());
			assertEquals("T", param2.getParameterType().getType().getIdentifier());
	
			var rt = act.getReturnType();
			assertNotNull(rt);
			assertSame(tp, rt.getType());
		}

	}

	@Nested
	@DisplayName("Skill created by builder")
	public class SkillTest extends AbstractSarlTest {

		@Inject
		private Injector injector;

		@Inject
		private CodeBuilderFactory factory;
	
		private ResourceSet resource;
	
		private IAppendable sourceAppender;

		private SarlSkillSourceAppender appender;
	
		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.appender = this.factory.buildSarlSkill("S", this.resource);

			final var imports = new ImportManager(true, this.appender.getJvmDeclaredType());
			this.injector.injectMembers(imports);
			this.sourceAppender = new FakeTreeAppendable(imports);
			this.injector.injectMembers(this.sourceAppender);
		}

		private void initSkill() {
			this.appender.setExtends(this.appender.newTypeRef(Skill.class));
			this.appender.addImplements(this.appender.newTypeRef(foo.bug1135.C.class));

			var fct0 = this.appender.addOverrideSarlAction("getData");
			var tp0 = fct0.addTypeParameter("T");
			fct0.addParameter("name").setParameterType(fct0.newTypeRef(String.class));
			fct0.addParameter("type").setParameterType(fct0.newTypeRef(Class.class, tp0.getJvmTypeParameterReference()));
			fct0.setReturnType(tp0.getJvmTypeParameterReference());
			fct0.getExpression().setDefaultAutoGeneratedContent();

			var fct1 = this.appender.addOverrideSarlAction("setData");
			final var tp1 = fct1.addTypeParameter("T");
			fct1.addParameter("name").setParameterType(fct1.newTypeRef(String.class));
			fct1.addParameter("type").setParameterType(fct1.newTypeRef(Class.class, tp1.getJvmTypeParameterReference()));
			fct1.addParameter("value").setParameterType(tp1.getJvmTypeParameterReference());
			fct1.getExpression().setDefaultAutoGeneratedContent();
		}
		
		private static final String SKILL_RESULT = multilineString2(false,
				"skill S implements foo.bug1135.C { override < T extends Object > getData ( name : String , type : Class < T > ) : T { ", 
				"// TODO Auto-generated code.",
				" } override < T extends Object > setData ( name : String , type : Class < T > , value : T ) { ",
				"// TODO Auto-generated code.",
				" } }"
				);
		
		@Test
		@DisplayName("Serialization without formatting")
		public void notFormattingSerialization() throws IOException {
			initSkill();
			this.appender.build(this.sourceAppender);
			assertEquals(SKILL_RESULT, this.sourceAppender.getContent());
		}
	
		@Test
		@DisplayName("Serialization with formatting")
		public void formattingSerialization() throws IOException {
			initSkill();
			this.appender.setFormatting(true);
			this.appender.build(this.sourceAppender);
			assertEquals(SKILL_RESULT, this.sourceAppender.getContent());
		}

	}

	@Nested
	@DisplayName("Script created by builder")
	public class ScriptTest extends AbstractSarlTest {

		@Inject
		private Injector injector;

		@Inject
		private CodeBuilderFactory factory;
	
		private ResourceSet resource;
	
		private IAppendable sourceAppender;

		private ScriptSourceAppender scriptAppender;
	
		private ISarlSkillBuilder skillAppender;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.scriptAppender = this.factory.buildScript("foo.bug1135", this.resource);
			this.skillAppender = this.scriptAppender.addSarlSkill("S");

			final var imports = new ImportManager(true, this.skillAppender.getJvmDeclaredType());
			this.injector.injectMembers(imports);
			this.sourceAppender = new FakeTreeAppendable(imports);
			this.injector.injectMembers(this.sourceAppender);
		}

		@Inject
		private TypeReferences typeReferences;

		private void initSkill() throws Exception {
			this.skillAppender.setExtends(this.skillAppender.newTypeRef(Skill.class));
			this.skillAppender.addImplements(this.skillAppender.newTypeRef(foo.bug1135.C.class));

			var fct0 = this.skillAppender.addOverrideSarlAction("getData");
			var tp0 = fct0.addTypeParameter("T");
			fct0.addParameter("name").setParameterType(fct0.newTypeRef(String.class));
			fct0.addParameter("type").setParameterType(fct0.newTypeRef(Class.class, tp0.getJvmTypeParameterReference()));
			fct0.setReturnType(tp0.getJvmTypeParameterReference());
			fct0.getExpression().setDefaultAutoGeneratedContent();

			var fct1 = this.skillAppender.addOverrideSarlAction("setData");
			var tp1 = fct1.addTypeParameter("T");
			fct1.addParameter("name").setParameterType(fct1.newTypeRef(String.class));
			fct1.addParameter("type").setParameterType(fct1.newTypeRef(Class.class, tp1.getJvmTypeParameterReference()));
			fct1.addParameter("value").setParameterType(tp1.getJvmTypeParameterReference());
			fct1.getExpression().setDefaultAutoGeneratedContent();
		}
		
		private static final String SCRIPT_RESULT = multilineString2(false,
				"package foo.bug1135 import foo.bug1135.C import io.sarl.lang.core.Skill skill S implements C { override < T extends Object > getData ( name : String , type : Class < T > ) : T { ", 
				"// TODO Auto-generated code.",
				" } override < T extends Object > setData ( name : String , type : Class < T > , value : T ) { ",
				"// TODO Auto-generated code.",
				" } }"
				);
		
		@Test
		@DisplayName("Serialization without formatting")
		public void notFormattingSerialization() throws Exception {
			initSkill();
			this.scriptAppender.build(this.sourceAppender);
			assertEquals(SCRIPT_RESULT, this.sourceAppender.getContent());
		}
	
		@Test
		@DisplayName("Serialization with formatting")
		public void formattingSerialization() throws Exception {
			initSkill();
			this.scriptAppender.setFormatting(true);
			this.scriptAppender.build(this.sourceAppender);
			assertEquals(SCRIPT_RESULT, this.sourceAppender.getContent());
		}

	}

	@Nested
	@DisplayName("Script generated by compiler")
	public class Script2Test extends AbstractSarlTest {

		private SarlScript script;

		@Inject
		private Injector injector;

		@Inject
		private CodeBuilderFactory factory;
	
		private ResourceSet resource;
	
		private IAppendable sourceAppender;

		private ScriptSourceAppender scriptAppender;

		@BeforeEach
		public void setUp() throws Exception {
			this.script = file(getParseHelper(), null, "abstract skill S implements foo.bug1135.C {"
					+ "override <T> setData(value : T) : void {}");

			this.resource = new ResourceSetImpl();
			this.scriptAppender = this.factory.buildScript("foo.bug1135", this.resource);

			final var imports = new ImportManager(true);
			this.injector.injectMembers(imports);
			this.sourceAppender = new FakeTreeAppendable(imports);
			this.injector.injectMembers(this.sourceAppender);
		}

		private static final String SCRIPT_RESULT = 
				"abstract   skill S   implements foo.bug1135.C   { override   < T > setData ( value   : T ) : void { } }";
		
		@Test
		@DisplayName("Serialization")
		public void serialization() throws Exception {
			this.scriptAppender.build(this.script, this.sourceAppender);
			assertEquals(SCRIPT_RESULT, this.sourceAppender.getContent());
		}

	}

}
