/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.docs.utils;

import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.Feature;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.TopElement;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.resource.ClassloaderClasspathUriResolver;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;
import org.eclipse.xtext.xbase.interpreter.IExpressionInterpreter;
import org.eclipse.xtext.xtype.XImportDeclaration;

import com.google.common.base.Predicate;
import com.google.inject.Inject;

/** Helper for accessing to the SARL parser.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
public class SARLParser {

	private static final double PRECISION = 10e-7;
	private static final int HEX_BASE = 16;

	@Inject
	private ParseHelper<SarlScript> sarlParser;
	@Inject
	private XtextResourceSet xtextResourceSet;
	@Inject
	private ValidationTestHelper validationTestHelper;
	@Inject
	private IExpressionInterpreter interpreter;

	private boolean initial = true;

	/** Parse a SARL code and replies the SARL model.
	 * <p>
	 * This function returns even if the SARL code is
	 * incorrect.
	 *
	 * @param text - the code to parse.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parse(CharSequence text) throws Exception {
		if (this.initial) {
			this.initial = false;
			return this.sarlParser.parse(text, getResourceSetWithDefaultModels());
		}
		return this.sarlParser.parse(text);
	}

	/** Expect a correct SARL code.
	 *
	 * @param text - the SARL code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parsesSuccessfully(CharSequence text) throws Exception {
		SarlScript model = parse(text);
		this.validationTestHelper.assertNoErrors(model);
		return model;
	}

	/** Expect an incorrect SARL code.
	 *
	 * @param text - the SARL code.
	 * @throws Exception - on parsing error.
	 */
	public void parsesWithError(CharSequence text) throws Exception {
		SarlScript model = parse(text);

		List<Issue> validate = this.validationTestHelper.validate(model);
		Iterable<Issue> issues = filter(validate, new Predicate<Issue>() {
			@Override
			public boolean apply(Issue input) {
				return Severity.ERROR == input.getSeverity();
			}
		});
		if (isEmpty(issues)) {
			fail("Expected error, but got no error."); //$NON-NLS-1$
		}
	}

	/** Expect a correct SARL code.
	 * <p>
	 * Contatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parsesSuccessfully(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		return parsesSuccessfully(b.toString());
	}

	/** Expect an incorrect SARL code.
	 * <p>
	 * Contatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @throws Exception - on parsing error.
	 */
	public void parsesWithError(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		parsesWithError(b.toString());
	}

	/** Expect a correct SARL code.
	 * <p>
	 * Contatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parsesSuccessfully(CharSequence outputText, CharSequence prefix, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			b.append(prefix);
			b.append("\n"); //$NON-NLS-1$
		}
		b.append(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		return parsesSuccessfully(b.toString());
	}

	/** Expect an incorrect SARL code.
	 * <p>
	 * Contatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @throws Exception - on parsing error.
	 */
	public void parsesWithError(CharSequence outputText, CharSequence prefix, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			b.append(prefix);
			b.append("\n"); //$NON-NLS-1$
		}
		b.append(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		parsesWithError(b.toString());
	}
	
	private ResourceSet getResourceSetWithDefaultModels() {
		this.xtextResourceSet.setClasspathURIContext(getClass());
		this.xtextResourceSet.setClasspathUriResolver(new ClassloaderClasspathUriResolver());
		this.xtextResourceSet.addLoadOption(XtextResource.OPTION_RESOLVE_ALL, Boolean.TRUE);
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/events.sarl")); //$NON-NLS-1$
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/bic.sarl")); //$NON-NLS-1$
		return this.xtextResourceSet;
	}

	/** Ensure that the given SARL script has a "package" statement.
	 *
	 * @param s - the SARL script.
	 * @param name - the name of the package.
	 * @return s
	 */
	public SarlScript mustHavePackage(SarlScript s, String name) {
		assertEquals("Missed package", name, s.getName()); //$NON-NLS-1$
		return s;
	}

	/** Ensure that the given SARL script has no import statement.
	 *
	 * @param s - the SARL script.
	 * @return s
	 */
	public SarlScript mustNotHaveImport(SarlScript s) {
		if (s.getImportSection() != null) {
			assertEquals("Must not have imports", 0, s.getImportSection().getImportDeclarations().size()); //$NON-NLS-1$
		}
		return s;
	}

	/** Ensure that the given SARL script has number of import statements.
	 *
	 * @param s - the SARL script.
	 * @param numberOfImports - the expected number of imports.
	 * @return s
	 */
	public SarlScript mustHaveImports(SarlScript s, int numberOfImports) {
		assertEquals("Invalid number of imports", //$NON-NLS-1$
				numberOfImports,
				s.getImportSection().getImportDeclarations().size());
		return s;
	}

	/** Ensure that the given SARL script has number of elements defined inside.
	 *
	 * @param s - the SARL script.
	 * @param numberOfElements - the number of top elements defined in the script.
	 * @return s
	 */
	public SarlScript mustHaveTopElements(SarlScript s, int numberOfElements) {
		assertEquals("Invalid number of imports", numberOfElements, s.getElements().size()); //$NON-NLS-1$
		return s;
	}

	/** Ensure that the given SARL script has an import statement.
	 *
	 * @param model - the SARL script.
	 * @param index - the expected position of the import statement in the list
	 * of the imports.
	 * @param name - the name of the imported type.
	 * @param isStatic - indicates if the type is statically imported.
	 * @param hasWildcard - indicates if the type is imported with a wildcard.
	 * @param isExtension - indicates if the type is imported as an extension.
	 * @return model
	 */
	public SarlScript mustHaveImport(
			SarlScript model, int index, String name, boolean isStatic,
			boolean hasWildcard, boolean isExtension) {
		XImportDeclaration d = model.getImportSection().getImportDeclarations().get(index);
		assertNotNull("Null import directive", d); //$NON-NLS-1$
		assertEquals("Not the same imported name", name, d.getImportedName()); //$NON-NLS-1$
		assertEquals("Invalid static modifier", isStatic, d.isStatic()); //$NON-NLS-1$
		assertEquals("Invalid wildcard flag", hasWildcard, d.isWildcard()); //$NON-NLS-1$
		assertEquals("Invalid extension flag", isExtension, d.isExtension()); //$NON-NLS-1$
		return model;
	}

	/** Ensure that the given object is the SARL "event" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the event.
	 * @param superType - the name of the expected super-type, or <code>null</code>
	 * if none.
	 * @return the agent
	 */
	public Event mustBeEvent(TopElement o, String name, String superType) {
		assertTrue("Not an event type", o instanceof Event); //$NON-NLS-1$
		Event e = (Event) o;
		assertEquals("Not same event name", name, e.getName()); //$NON-NLS-1$
		if (superType != null) {
			assertEquals("Invalid number of super-types", 1, e.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, e.getSuperTypes().get(0).getQualifiedName()); //$NON-NLS-1$
		} else {
			assertEquals("Invalid number of super-types", 0, e.getSuperTypes().size()); //$NON-NLS-1$
		}
		return e;
	}

	/** Ensure that the given object is the SARL "capacity" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the capacity.
	 * @param superTypes - the list of the names of the super-types.
	 * @return o
	 */
	public Capacity mustBeCapacity(TopElement o, String name, String... superTypes) {
		assertTrue("Not a capacity type", o instanceof Capacity); //$NON-NLS-1$
		Capacity c = (Capacity) o;
		assertEquals("Invalid capacity name", name, c.getName()); //$NON-NLS-1$
		Set<String> set = new TreeSet<>(Arrays.asList(superTypes));
		assertEquals("Invalid number of super-types", superTypes.length, c.getSuperTypes().size()); //$NON-NLS-1$
		for (JvmParameterizedTypeReference ref : c.getSuperTypes()) {
			assertTrue("Unexpected super-type: " + ref.getQualifiedName(), set.remove(ref.getQualifiedName())); //$NON-NLS-1$
		}
		return c;
	}

	/** Ensure that the given object is the SARL "skill" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the skill.
	 * @param superType - the name of the expected super-type, or <code>null</code>
	 * if none.
	 * @param capacities - the list of the names of the implemented capacities.
	 * @return o
	 */
	public Skill mustBeSkill(TopElement o, String name, String superType, String... capacities) {
		assertTrue("Invalid type of skill", o instanceof Skill); //$NON-NLS-1$
		Skill s = (Skill) o;
		assertEquals("Not same skill name", name, s.getName()); //$NON-NLS-1$
		if (superType != null) {
			assertEquals("Invalid number of super-types", 1, s.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, s.getSuperTypes().get(0).getQualifiedName()); //$NON-NLS-1$
		} else {
			assertEquals(0, s.getSuperTypes().size());
		}
		Set<String> set = new TreeSet<>(Arrays.asList(capacities));
		assertEquals("Invalid number of capacities", capacities.length, s.getImplementedTypes().size()); //$NON-NLS-1$
		for (JvmParameterizedTypeReference ref : s.getImplementedTypes()) {
			assertTrue("Unexpected capacity: " + ref.getQualifiedName(), set.remove(ref.getQualifiedName())); //$NON-NLS-1$
		}
		return s;
	}

	/** Ensure that the given object is the SARL "behavior" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the behavior.
	 * @param superType - the name of the expected super-type, or <code>null</code>
	 * if none.
	 * @return o
	 */
	public Behavior mustBeBehavior(TopElement o, String name, String superType) {
		assertTrue("Invalid type of behavior", o instanceof Behavior); //$NON-NLS-1$
		Behavior b = (Behavior) o;
		assertEquals("Invalid behavior name", name, b.getName()); //$NON-NLS-1$
		if (superType != null) {
			assertEquals("Invalid number of super-types", 1, b.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, b.getSuperTypes().get(0).getQualifiedName()); //$NON-NLS-1$
		} else {
			assertEquals("Invalid number of super-types", 0, b.getSuperTypes().size()); //$NON-NLS-1$
		}
		return b;
	}

	/** Ensure that the given object is the SARL "agent" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @param superType - the name of the expected super-type, or <code>null</code>
	 * if none.
	 * @return o
	 */
	public Agent mustBeAgent(TopElement o, String name, String superType) {
		assertTrue("Invalid type of agent", o instanceof Agent); //$NON-NLS-1$
		Agent a = (Agent) o;
		assertEquals("Invalid agent name", name, a.getName()); //$NON-NLS-1$
		if (superType != null) {
			assertEquals("Invalid number of super-types", 1, a.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, a.getSuperTypes().get(0).getQualifiedName()); //$NON-NLS-1$
		} else {
			assertEquals("Invalid number of super-types", 0, a.getSuperTypes().size()); //$NON-NLS-1$
		}
		return a;
	}

	/** Ensure that the given object has the given number of features inside.
	 *
	 * @param <T> - type of the object.
	 * @param c - the object to test.
	 * @param numberOfFeatures - the expected number of features.
	 * @return c
	 */
	public <T extends FeatureContainer> T mustHaveFeatures(T c, int numberOfFeatures) {
		assertEquals("Invalid number of features", numberOfFeatures, c.getFeatures().size()); //$NON-NLS-1$
		return c;
	}

	/** Ensure that the given object is the SARL "var" or "val" statement.
	 *
	 * @param o - the object to test.
	 * @param writable - indicates if the attribute is writeable ("var") or not ("val").
	 * @param name - the expected name of the attribute.
	 * @param type - the name of the expected type of the attribute, or <code>null</code> if inferred.
	 * @param expression - indicates if the attribute is initialized.
	 * @return the attribute
	 */
	public Attribute mustBeAttribute(EObject o, boolean writable, String name, String type, boolean expression) {
		assertTrue("Invalid type of attribute", o instanceof Attribute); //$NON-NLS-1$
		Attribute attr = (Attribute) o;
		assertNotNull("Attribute is null", attr); //$NON-NLS-1$
		assertEquals("Invalid name of attribute", name, attr.getName()); //$NON-NLS-1$
		assertEquals("Invalid writable modifier", writable, attr.isWriteable()); //$NON-NLS-1$
		if (type == null) {
			assertNull("Unexpected attribute type", attr.getType()); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting attribute type", attr.getType()); //$NON-NLS-1$
			assertEquals("Unexpected attribute type", type, attr.getType().getQualifiedName()); //$NON-NLS-1$
		}
		assertEquals("Invalid attribute expression", expression, attr.getInitialValue() != null); //$NON-NLS-1$
		return attr;
	}

	/** Ensure that the given object is the SARL "def" statement (with body).
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the action.
	 * @param returnType - the name of the expectd return type.
	 * @param numberOfParameters - the expected number of formal parameters.
	 * @param varargs - indicates if the action is variadic.
	 * @return the action
	 */
	public Action mustBeAction(EObject o, String name, String returnType, int numberOfParameters, boolean varargs) {
		assertTrue("Invalid type of action", o instanceof Action); //$NON-NLS-1$
		Action act = (Action) o;
		ParameterizedFeature pf = act.getSignature();
		assertTrue("Invalid type of action signature", pf instanceof ActionSignature); //$NON-NLS-1$
		ActionSignature sig = (ActionSignature) pf;
		assertEquals("Invalid name of action", name, sig.getName()); //$NON-NLS-1$
		if (returnType == null) {
			assertNull("Unexpected return type", sig.getType()); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting return type", sig.getType()); //$NON-NLS-1$
			assertEquals("Invalid return type", returnType, sig.getType().getQualifiedName()); //$NON-NLS-1$
		}
		assertNotNull("Expecting action body", act.getBody()); //$NON-NLS-1$
		assertEquals("Invalid number of formal parameters", numberOfParameters, sig.getParams().size()); //$NON-NLS-1$
		assertEquals("Invalid variadic modifier", varargs, sig.isVarargs()); //$NON-NLS-1$
		return act;
	}

	/** Ensure that the given object is the SARL "def" statement (without body).
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the action.
	 * @param returnType - the name of the expectd return type.
	 * @param numberOfParameters - the expected number of formal parameters.
	 * @param varargs - indicates if the action is variadic.
	 * @return the action
	 */
	public ActionSignature mustBeActionSignature(
			EObject o, String name, String returnType,
			int numberOfParameters, boolean varargs) {
		assertTrue("Invalid type of action signature", o instanceof ActionSignature); //$NON-NLS-1$
		ActionSignature sig = (ActionSignature) o;
		assertEquals("Invalid actrion signature name", name, sig.getName()); //$NON-NLS-1$
		if (returnType == null) {
			assertNull("Unexpected return type", sig.getType()); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting return type", sig.getType()); //$NON-NLS-1$
			assertEquals("Invalid return type", returnType, sig.getType().getQualifiedName()); //$NON-NLS-1$
		}
		assertEquals("Invalid number of formal parameters", numberOfParameters, sig.getParams().size()); //$NON-NLS-1$
		assertEquals("Invalid variadic flag", varargs, sig.isVarargs()); //$NON-NLS-1$
		return sig;
	}

	/** Ensure that the given object is the SARL constructor.
	 *
	 * @param o - the object to test.
	 * @param numberOfParameters - the expected number of formal parameters for the constructor.
	 * @param varargs - indicates if the constructor is variadic.
	 * @return the action signature
	 */
	public Constructor mustBeConstructor(EObject o, int numberOfParameters, boolean varargs) {
		assertTrue("Invalid constructor type", o instanceof Constructor); //$NON-NLS-1$
		Constructor cons = (Constructor) o;
		assertNotNull("Exepcting body constructor", cons.getBody()); //$NON-NLS-1$
		assertEquals("Invalid number of formal parameters", numberOfParameters, cons.getParams().size()); //$NON-NLS-1$
		assertEquals("Invalid variadic flag", varargs, cons.isVarargs()); //$NON-NLS-1$
		return cons;
	}

	/** Ensure that the given object is the SARL "on" statement.
	 *
	 * @param o - the object to test.
	 * @param eventQualifiedName - the qualified name of the expected event.
	 * @param guard - indicates if a guard must be defined.
	 * @return the unit
	 */
	public BehaviorUnit mustBeBehaviorUnit(EObject o, String eventQualifiedName, boolean guard) {
		assertTrue("Invalid type of behavior unit", o instanceof BehaviorUnit); //$NON-NLS-1$
		BehaviorUnit bu = (BehaviorUnit) o;
		assertEquals("Invalid event name", eventQualifiedName, bu.getEvent().getQualifiedName()); //$NON-NLS-1$
		assertEquals("Invalid guard", guard, (bu.getGuard() != null)); //$NON-NLS-1$
		assertNotNull("Expecting behavior unit body", bu.getBody()); //$NON-NLS-1$
		return bu;
	}

	/** Ensure that the given object is the SARL "uses" statement.
	 *
	 * @param o - the object to test.
	 * @param capacityQualifiedName - the list of the qualified names of
	 * the capacites that must follow the keyword "uses".
	 * @return the statement
	 */
	public CapacityUses mustBeCapacityUses(EObject o, String... capacityQualifiedName) {
		assertTrue("Invalid type of capacity uses", o instanceof CapacityUses); //$NON-NLS-1$
		CapacityUses cu = (CapacityUses) o;
		assertTrue("not enough parameters for the function mustBeCapacityUses", //$NON-NLS-1$
				capacityQualifiedName.length > 0);
		assertEquals("Invalid number of capacities", cu.getCapacitiesUsed().size(), //$NON-NLS-1$
				capacityQualifiedName.length);
		Set<String> elements = new TreeSet<>(Arrays.asList(capacityQualifiedName));
		for (JvmParameterizedTypeReference t : cu.getCapacitiesUsed()) {
			if (t != null) {
				assertTrue("not expecting capacity: " + t.getQualifiedName(), //$NON-NLS-1$
						elements.remove(t.getQualifiedName()));
			}
		}
		assertTrue("Expecting capacities: " + elements, elements.isEmpty()); //$NON-NLS-1$
		return cu;
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param <T> - type of the object.
	 * @param o - the feature to test.
	 * @param index - the expected position of the parameter in the list of parameters.
	 * @param name - the expected name of the formal parameter.
	 * @param type - the name of the expected type of the formal parameter.
	 * @param defaultValue - indicates if a default value must be defined.
	 * @return o
	 */
	public <T extends Feature> T mustHaveParameter(T o, int index, String name, String type, boolean defaultValue) {
		ParameterizedFeature pf;
		if (o instanceof Action) {
			pf = ((Action) o).getSignature();
		} else {
			pf = (ParameterizedFeature) o;
		}
		FormalParameter p = pf.getParams().get(index);
		assertNotNull("Expecting formal parameter", p); //$NON-NLS-1$
		assertEquals("Invalid parameter name", name, p.getName()); //$NON-NLS-1$
		if (type == null) {
			assertNull("Unexpected parameter type", p.getParameterType()); //$NON-NLS-1$
			assertNotNull("Expecting default value", p.getDefaultValue()); //$NON-NLS-1$
			assertTrue("Expecting default value", defaultValue); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting parameter type", p.getParameterType()); //$NON-NLS-1$
			assertEquals("Invalid parameter type", type, p.getParameterType().getQualifiedName()); //$NON-NLS-1$
			assertEquals("Invalid default value", defaultValue, p.getDefaultValue() != null); //$NON-NLS-1$
		}
		return o;
	}

	/** Ensure that the given string literal is equal to the given value.
	 *
	 * @param actual - the string literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XStringLiteral mustBeEqual(XStringLiteral actual, String expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getValue()); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given string literal is equal to the given value.
	 *
	 * @param actual - the string literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XStringLiteral mustBeEqual(XStringLiteral actual, char expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getValue()); //$NON-NLS-1$
		assertEquals("not equal", 1, actual.getValue().length()); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getValue().charAt(0)); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given boolean literal is equal to the given value.
	 *
	 * @param actual - the boolean literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XBooleanLiteral mustBeEqual(XBooleanLiteral actual, boolean expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.isIsTrue()); //$NON-NLS-1$
		return actual;
	}

	private String cleanNumber(String s) {
		if (s == null) {
			return null;
		}
		if (s.startsWith("0x") || s.startsWith("0X")) { //$NON-NLS-1$//$NON-NLS-2$
			return s;
		}
		String literal = s.replace("_", ""); //$NON-NLS-1$//$NON-NLS-2$
		literal = literal.toLowerCase().replaceFirst("l|f|d|(bi)|(bd)$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		return literal;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, Number expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected.toString(), cleanNumber(actual.getValue())); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, byte expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Byte.parseByte(cleanNumber(actual.getValue()))); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, short expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Short.parseShort(cleanNumber(actual.getValue()))); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, int expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getValue()); //$NON-NLS-1$
		String literal = cleanNumber(actual.getValue());
		if (literal.startsWith("0x") || literal.startsWith("0X")) { //$NON-NLS-1$ //$NON-NLS-2$
			assertEquals("not equal", expected, Integer.parseInt(literal.substring(2), HEX_BASE)); //$NON-NLS-1$
		} else {
			assertEquals("not equal", expected, Integer.parseInt(literal)); //$NON-NLS-1$
		}
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, long expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		String literal = cleanNumber(actual.getValue());
		if (literal.startsWith("0x") || literal.startsWith("0X")) { //$NON-NLS-1$ //$NON-NLS-2$
			assertEquals("not equal", expected, Long.parseLong(literal.substring(2), HEX_BASE)); //$NON-NLS-1$
		} else {
			assertEquals("not equal", expected, Long.parseLong(literal)); //$NON-NLS-1$
		}
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, float expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Float.parseFloat(cleanNumber(actual.getValue())), PRECISION); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, double expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Double.parseDouble(cleanNumber(actual.getValue())), PRECISION); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given type literal is equal to the given type.
	 *
	 * @param actual - the type literal to test.
	 * @param expected - the name of the expected type.
	 * @return actual
	 */
	public XTypeLiteral mustBeEqual(XTypeLiteral actual, String expected) {
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getType()); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getType().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given type literal is equal to the given type.
	 *
	 * @param actual - the type literal to test.
	 * @param expected - the expected type.
	 * @return actual
	 */
	public XTypeLiteral mustBeEqual(XTypeLiteral actual, Class<?> expected) {
		return mustBeEqual(actual, expected.getName());
	}

	/** Ensure that the given feature call is refering the given type.
	 *
	 * @param actual - the feature call to test.
	 * @param expected - the expected refered type.
	 * @return actual
	 */
	public XFeatureCall mustBeType(XFeatureCall actual, Class<?> expected) {
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertTrue("Feature call is not a type literal", actual.isTypeLiteral()); //$NON-NLS-1$
		assertEquals("Invalid type", expected.getName(), actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given feature call is calling the feature with
	 * the given name.
	 *
	 * @param actual - the feature call to test.
	 * @param expected - the expected name of the feature.
	 * @return actual
	 */
	public XMemberFeatureCall mustCall(XMemberFeatureCall actual, String expected) {
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertEquals("Invalid type", expected, actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given feature call is calling the feature with
	 * the given name.
	 *
	 * @param actual - the feature call to test.
	 * @param expected - the extected name of the feature.
	 * @return actual
	 */
	public XFeatureCall mustCall(XFeatureCall actual, String expected) {
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertEquals("Invalid type", expected, actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/** Parse a Xbase expression.
	 * 
	 * @param expression - the expression to parse.
	 * @return the XExpression that is corresponding to the given expression.
	 * @throws Exception
	 */
	public XExpression expression(String expression) throws Exception {
		return expression(expression, true);
	}
	
	/** Parse a Xbase expression.
	 * 
	 * @param expression - the expression to parse.
	 * @param resolve - <code>true</code> if the expression must have no error, <code>false</code>
	 * to not care.
	 * @return the XExpression that is corresponding to the given expression.
	 * @throws Exception
	 */
	public XExpression expression(String expression, boolean resolve) throws Exception {
		String code = "def ____TeStInG_FuNcTiOn() : Object {\n" //$NON-NLS-1$
				+ expression
				+ "\n}"; //$NON-NLS-1$
		Action action = (Action) agentCode("AgentXXXXX", code, resolve).get(0); //$NON-NLS-1$
		XBlockExpression block = (XBlockExpression) action.getBody();
		if (block.getExpressions().size() == 1) {
			return block.getExpressions().get(0);
		}
		return block;

	}

	/** Parse the code of an agent (attributes, actions, behavior units...).
	 * 
	 * @param agentTypeName - name of the type of agent.
	 * @param code - the code to parse.
	 * @param resolve - <code>true</code> if the code must have no error, <code>false</code>
	 * to not care.
	 * @return the statements in the agent definition.
	 * @throws Exception
	 */
	public List<EObject> agentCode(String agentTypeName, String code, boolean resolve) throws Exception {
		String fullCode = "agent " + agentTypeName //$NON-NLS-1$
				+ " {\n" + code //$NON-NLS-1$
				+ "\n}\n"; //$NON-NLS-1$
		SarlScript script = parse(fullCode);
		if (resolve) {
			this.validationTestHelper.assertNoErrors(script);
		}
		Agent agent = (Agent) script.getElements().get(0);
		return agent.getFeatures();
	}

	/** Evaluate an expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @param resultType - the expected type of the result.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public <T> T to(String expression, Class<T> resultType) throws Exception {
		XExpression expr = expression(expression);
		IEvaluationResult r = this.interpreter.evaluate(expr);
		if (r == null) {
			throw new RuntimeException("cannot evaluate"); //$NON-NLS-1$
		}
		Throwable e = r.getException();
		if (e != null) {
			if (e instanceof Exception) {
				throw (Exception)e;
			}
			else if (e instanceof Error) {
				throw (Error)e;
			}
			throw new RuntimeException(e);
		}
		Object v = r.getResult();
		if (v == null) {
			return null;
		}
		if (resultType.isInstance(v)) {
			return resultType.cast(v);
		}
		fail("Invalid type. Expected: " //$NON-NLS-1$
				+ resultType.getName() + ", but was: " //$NON-NLS-1$
				+ v.getClass().getName());
		return null;
	}
	
	/** Evaluate a byte expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public byte toByte(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.byteValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a short integer expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public short toShort(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.shortValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate an integer expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public int toInt(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.intValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a long integer expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public long toLong(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.longValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a single-precision floating point expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public float toFloat(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.floatValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a double-precision floating point expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public double toDouble(String expression) throws Exception {
		Number n = to(expression, Number.class);
		if (n != null) {
			return n.doubleValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a character expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public char toChar(String expression) throws Exception {
		Character n = to(expression, Character.class);
		if (n != null) {
			return n.charValue();
		}
		fail("Illegal character"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a boolean expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public boolean toBool(String expression) throws Exception {
		Boolean n = to(expression, Boolean.class);
		if (n != null) {
			return n.booleanValue();
		}
		fail("Illegal boolean value"); //$NON-NLS-1$
		return false;
	}

	/** Evaluate a boolean expression and reply the result.
	 * 
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception
	 */
	public String toStr(String expression) throws Exception {
		Object n = to(expression, Object.class);
		if (n != null) {
			return n.toString();
		}
		return null;
	}

}
