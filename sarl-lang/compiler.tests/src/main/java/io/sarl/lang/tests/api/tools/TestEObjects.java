/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.api.tools;

import java.util.Arrays;
import java.util.List;

import com.google.inject.Provider;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.tests.api.tools.TestUtils;

/** Set of additional utilities for created testing EObject, except those related to SARL concepts.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class TestEObjects {

	private TestEObjects() {
		//
	}

	/** Create an instance of agent.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL agent.
	 * @throws Exception 
	 */
	public static SarlAgent agent(ParseHelper<SarlScript> parser, String string) throws Exception {
		return agent(parser, null, string);
	}

	/** Create an instance of agent.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL agent.
	 * @throws Exception 
	 */
	public static SarlAgent agent(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlAgent) decls.get(decls.size() - 1);
	}

	/** Create an instance of capacity.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL capacity.
	 * @throws Exception 
	 */
	public static SarlCapacity capacity(ParseHelper<SarlScript> parser, String string) throws Exception {
		return capacity(parser, null, string);
	}

	/** Create an instance of capacity.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL capacity.
	 * @throws Exception 
	 */
	public static SarlCapacity capacity(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlCapacity) decls.get(decls.size() - 1);
	}

	/** Create an instance of event.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL event.
	 * @throws Exception 
	 */
	public static SarlEvent event(ParseHelper<SarlScript> parser, String string) throws Exception {
		return event(parser, null, string);
	}

	/** Create an instance of event.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL event.
	 * @throws Exception 
	 */
	public static SarlEvent event(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlEvent) decls.get(decls.size() - 1);
	}

	/** Create an instance of skill.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL skill.
	 * @throws Exception 
	 */
	public static SarlSkill skill(ParseHelper<SarlScript> parser, String string) throws Exception {
		return skill(parser, null, string);
	}

	/** Create an instance of skill.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL skill.
	 * @throws Exception 
	 */
	public static SarlSkill skill(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlSkill) decls.get(decls.size() - 1);
	}

	/** Create an instance of behavior.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL behavior.
	 * @throws Exception 
	 */
	public static SarlBehavior behavior(ParseHelper<SarlScript> parser, String string) throws Exception {
		return behavior(parser, null, string);
	}

	/** Create an instance of behavior.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL behavior.
	 * @throws Exception 
	 */
	public static SarlBehavior behavior(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlBehavior) decls.get(decls.size() - 1);
	}

	/** Create an instance of class.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL class.
	 * @throws Exception 
	 */
	public static SarlClass clazz(ParseHelper<SarlScript> parser, String string) throws Exception {
		return clazz(parser, null, string);
	}

	/** Create an instance of class.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL class.
	 * @throws Exception 
	 */
	public static SarlClass clazz(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlClass) decls.get(decls.size() - 1);
	}

	/** Create an instance of annotation type.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL annotation.
	 * @throws Exception 
	 */
	public static SarlAnnotationType annotationType(ParseHelper<SarlScript> parser, String string) throws Exception {
		return annotationType(parser, null, string);
	}

	/** Create an instance of annotation type.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL annotation.
	 * @throws Exception 
	 */
	public static SarlAnnotationType annotationType(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlAnnotationType) decls.get(decls.size() - 1);
	}

	/** Create an instance of interface.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL interface.
	 * @throws Exception 
	 */
	public static SarlInterface interfaze(ParseHelper<SarlScript> parser, String string) throws Exception {
		return interfaze(parser, null, string);
	}

	/** Create an instance of interface.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL interface.
	 * @throws Exception 
	 */
	public static SarlInterface interfaze(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlInterface) decls.get(decls.size() - 1);
	}

	/** Create an instance of enumeration.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @return the SARL enumeration.
	 * @throws Exception 
	 */
	public static SarlEnumeration enumeration(ParseHelper<SarlScript> parser, String string) throws Exception {
		return enumeration(parser, null, string);
	}

	/** Create an instance of enumeration.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @return the SARL enumeration.
	 * @throws Exception 
	 */
	public static SarlEnumeration enumeration(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		List<XtendTypeDeclaration> decls = io.sarl.tests.api.tools.TestEObjects.file(parser, validationHelper, string).getXtendTypes();
		return (SarlEnumeration) decls.get(decls.size() - 1);
	}

	/** Create an instance of function.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL function.
	 * @throws Exception 
	 */
	public static SarlAction function(ParseHelper<SarlScript> parser, String string, String... prefix) throws Exception {
		return function(parser, null, string, prefix);
	}

	/** Create an instance of function.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL function.
	 * @throws Exception 
	 */
	public static SarlAction function(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper, String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(parser, validationHelper,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "class Foo { " + string + "}"); //$NON-NLS-1$ //$NON-NLS-2$
		return (SarlAction) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM operation.
	 * @throws Exception 
	 */
	public static JvmOperation jvmOperation(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			String string, String... prefix) throws Exception {
		return jvmOperation(parser, associations, null, string, prefix);
	}

	/** Create an instance of JVM function.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM operation.
	 * @throws Exception 
	 */
	public static JvmOperation jvmOperation(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			ValidationTestHelper validationHelper, String string, String... prefix) throws Exception {
		assert associations != null;
		SarlAction action = function(parser, validationHelper, string, prefix);
		return (JvmOperation) associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of function signature.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL action.
	 * @throws Exception 
	 */
	public static SarlAction functionSignature(ParseHelper<SarlScript> parser, String string, String... prefix) throws Exception {
		return functionSignature(parser, null, string, prefix);
	}

	/** Create an instance of function signature.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL action.
	 * @throws Exception 
	 */
	public static SarlAction functionSignature(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper,
			String string, String... prefix) throws Exception {
		SarlInterface interfaze = interfaze(parser, validationHelper,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "interface Foo { " + string + "}"); //$NON-NLS-1$ //$NON-NLS-2$
		return (SarlAction) interfaze.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM operation signature.
	 * @throws Exception 
	 */
	public static JvmOperation jvmOperationSignature(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			String string, String... prefix) throws Exception {
		return jvmOperationSignature(parser, associations, null, string, prefix);
	}

	/** Create an instance of JVM function.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM operation signature.
	 * @throws Exception 
	 */
	public static JvmOperation jvmOperationSignature(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			ValidationTestHelper validationHelper, String string, String... prefix) throws Exception {
		assert associations != null;
		SarlAction action = functionSignature(parser, validationHelper, string, prefix);
		return (JvmOperation) associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of constructor.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL constructor.
	 * @throws Exception 
	 */
	public static SarlConstructor constructor(ParseHelper<SarlScript> parser, String string, String... prefix) throws Exception {
		return constructor(parser, null, string, prefix);
	}

	/** Create an instance of constructor.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL constructor.
	 * @throws Exception 
	 */
	public static SarlConstructor constructor(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper,
			String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(parser, validationHelper,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "class Foo { " + string + "}"); //$NON-NLS-1$ //$NON-NLS-2$
		return (SarlConstructor) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM constructor.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM constructor.
	 * @throws Exception 
	 */
	public static JvmConstructor jvmConstructor(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			String string, String... prefix) throws Exception {
		return jvmConstructor(parser, associations, null, string, prefix);
	}

	/** Create an instance of JVM constructor.
	 *
	 * @param parser the SARL parser.
	 * @param associations the provider of associations between the JVM and the SARL Ecores.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the JVM constructor.
	 * @throws Exception 
	 */
	public static JvmConstructor jvmConstructor(ParseHelper<SarlScript> parser, Provider<SarlJvmModelAssociations> associations,
			ValidationTestHelper validationHelper, String string, String... prefix) throws Exception {
		assert associations != null;
		SarlConstructor constructor = constructor(parser, validationHelper, string, prefix);
		return (JvmConstructor) associations.get().getPrimaryJvmElement(constructor);
	}

	/** Create an instance of field.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL field.
	 * @throws Exception 
	 */
	public static SarlField field(ParseHelper<SarlScript> parser, String string, String... prefix) throws Exception {
		return field(parser, null, string, prefix);
	}

	/** Create an instance of field.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL field.
	 * @throws Exception 
	 */
	public static SarlField field(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper,
			String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(parser, validationHelper,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "class Foo { " + string + "}"); //$NON-NLS-1$ //$NON-NLS-2$
		return (SarlField) clazz.getMembers().get(0);
	}

	/** Create an instance of behavior unit.
	 *
	 * @param parser the SARL parser.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL behavior unit.
	 * @throws Exception 
	 */
	public static SarlBehaviorUnit behaviorUnit(ParseHelper<SarlScript> parser, String string, String... prefix) throws Exception {
		return behaviorUnit(parser, null, string, prefix);
	}

	/** Create an instance of behavior unit.
	 *
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the file content to parse.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the SARL behavior unit.
	 * @throws Exception 
	 */
	public static SarlBehaviorUnit behaviorUnit(ParseHelper<SarlScript> parser, ValidationTestHelper validationHelper,
			String string, String... prefix) throws Exception {
		SarlAgent agent = agent(parser, validationHelper,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "agent Foo { " + string + "}"); //$NON-NLS-1$ //$NON-NLS-2$
		return (SarlBehaviorUnit) agent.getMembers().get(0);
	}

	/** Create a type reference with the SARL parser.
	 *
	 * @param parser the SARL parser.
	 * @param typeName the fully qualified name of the type.
	 * @param prefix the set of lines to put before the class declaration of the function.
	 * @return the type reference.
	 * @throws Exception 
	 */
	public static JvmTypeReference getType(ParseHelper<SarlScript> parser, String typeName, String... prefix) throws Exception {
		SarlAgent agent = agent(parser,
				IterableExtensions.join(Arrays.asList(prefix), TestUtils.getLineSeparator())
				+ TestUtils.getLineSeparator() + "agent Foo { var fooAttr : " + typeName + " }"); //$NON-NLS-1$ //$NON-NLS-2$
		return ((SarlField) agent.getMembers().get(0)).getType();
	}

}
