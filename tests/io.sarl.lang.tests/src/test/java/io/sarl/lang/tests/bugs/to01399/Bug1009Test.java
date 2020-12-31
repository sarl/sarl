/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Expression with side effect is not allowed in guards.
 *
 * <p>https://github.com/sarl/sarl/issues/1009
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1009"
 */
@DisplayName("Bug #1009")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlParsing")
public class Bug1009Test extends AbstractSarlTest {

	private static final String SARL_CODE_01a = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"	def MT_getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	MT_getEntityState(occurrence.entityName).routeLength == 0 &&", 
			"		MT_getEntityState(occurrence.entityName).facility !== null &&", 
			"		state.getShops().containsKey(MT_getEntityState(occurrence.entityName).facility) &&", 
			"		MT_getEntityState(occurrence.entityName).lastAction.type != \"buy\"",
			"  ] {}",
			"}");

	@Test
	@DisplayName("personal name w/o braces w/o pure annotation")
	public void parsing01a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertError(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				IssueCodes.INVALID_INNER_EXPRESSION,
				"side effect", "not allowed", "in guards");
	}

	private static final String SARL_CODE_01b = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"   @Pure",
			"	def MT_getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	MT_getEntityState(occurrence.entityName).routeLength == 0 &&", 
			"		MT_getEntityState(occurrence.entityName).facility !== null &&", 
			"		state.getShops().containsKey(MT_getEntityState(occurrence.entityName).facility) &&", 
			"		MT_getEntityState(occurrence.entityName).lastAction.type != \"buy\"",
			"  ] {}",
			"}");

	@Test
	@DisplayName("personal name w/o braces w/ pure annotation")
	public void parsing01b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_02a = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"	def getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	getEntityState(occurrence.entityName).routeLength == 0 &&", 
			"		getEntityState(occurrence.entityName).facility !== null &&", 
			"		state.getShops().containsKey(getEntityState(occurrence.entityName).facility) &&", 
			"		getEntityState(occurrence.entityName).lastAction.type != \"buy\"",
			"  ] {}",
			"}");

	@Test
	@DisplayName("standard name w/o braces w/o pure annotation")
	public void parsing02a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_02b = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"   @Pure",
			"	def getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	getEntityState(occurrence.entityName).routeLength == 0 &&", 
			"		getEntityState(occurrence.entityName).facility !== null &&", 
			"		state.getShops().containsKey(getEntityState(occurrence.entityName).facility) &&", 
			"		getEntityState(occurrence.entityName).lastAction.type != \"buy\"",
			"  ] {}",
			"}");

	@Test
	@DisplayName("standard name w/o braces w/ pure annotation")
	public void parsing02b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_03a = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"	def MT_getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	{", 
			"	val entity = MT_getEntityState(occurrence.entityName)",
			"	entity.routeLength == 0 && entity.facility !== null &&",
			"		state.getShops().containsKey(entity.facility) && entity.lastAction.type != \"buy\"",
			"   }] {}",
			"}");

	@Test
	@DisplayName("personal name w/ braces w/o pure annotation")
	public void parsing03a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertError(
				XbasePackage.eINSTANCE.getXBlockExpression(),
				IssueCodes.INVALID_INNER_EXPRESSION,
				"side effect", "not allowed", "in guards");
	}

	private static final String SARL_CODE_03b = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"   @Pure",
			"	def MT_getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	{", 
			"	val entity = MT_getEntityState(occurrence.entityName)",
			"	entity.routeLength == 0 && entity.facility !== null &&",
			"		state.getShops().containsKey(entity.facility) && entity.lastAction.type != \"buy\"",
			"   }] {}",
			"}");

	@Test
	@DisplayName("personal name w/ braces w/ pure annotation")
	public void parsing03b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_04a = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"	def getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	{", 
			"	val entity = getEntityState(occurrence.entityName)",
			"	entity.routeLength == 0 && entity.facility !== null &&",
			"		state.getShops().containsKey(entity.facility) && entity.lastAction.type != \"buy\"",
			"   }] {}",
			"}");

	@Test
	@DisplayName("standard name w/ braces w/o pure annotation")
	public void parsing04a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_04b = multilineString(
			"package io.sarl.lang.tests.bug1009",
			"import java.util.Map",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"event E_EntitySensed {",
			"  var entityName : String",
			"}",
			"interface Action {",
			"  def getType : String",
			"}",
			"interface EntityData {",
			"  def getRouteLength : double",
			"  def getFacility : Object",
			"  def getLastAction : Action",
			"}",
			"capacity C_MassimTalking {",
			"   @Pure",
			"	def getEntityState(playerName : String) : EntityData",
			"}",
			"interface State {",
			"  def getShops : Map<Object, Object>",
			"}",
			"agent Bug1009Case {",
			"  uses C_MassimTalking",
			"  var state : State",
			"  on E_EntitySensed [",
			"	{", 
			"	val entity = getEntityState(occurrence.entityName)",
			"	entity.routeLength == 0 && entity.facility !== null &&",
			"		state.getShops().containsKey(entity.facility) && entity.lastAction.type != \"buy\"",
			"   }] {}",
			"}");

	@Test
	@DisplayName("standard name w/ braces w/ pure annotation")
	public void parsing04b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

}
