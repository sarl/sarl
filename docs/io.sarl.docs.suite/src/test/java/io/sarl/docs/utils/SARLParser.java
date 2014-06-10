/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
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
package io.sarl.docs.utils;

import static com.google.common.collect.Iterables.contains;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.Capacity;
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
import java.util.Set;
import java.util.TreeSet;

import junit.framework.AssertionFailedError;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.resource.ClassloaderClasspathUriResolver;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.hamcrest.Matcher;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
public class SARLParser {

	@Inject
	private ParseHelper<SarlScript> parser;
	@Inject
	private XtextResourceSet xtextResourceSet;
	@Inject
	private ValidationTestHelper validationTestHelper;

	/**
	 * @param text
	 * @return the model.
	 * @throws Exception
	 */
	public SarlScript parse(CharSequence text) throws Exception {
		return this.parser.parse(text, getResourceSetWithDeafaultModels());
	}
	
	/**
	 * 
	 * @param text
	 * @return the model.
	 * @throws Exception
	 */
	public SarlScript parsesSuccessfully(CharSequence text) throws Exception{
		SarlScript model = parse(text);
		this.validationTestHelper.assertNoErrors(model);
		return model;
	}

	private ResourceSet getResourceSetWithDeafaultModels() {
		this.xtextResourceSet.setClasspathURIContext(getClass());
		this.xtextResourceSet.setClasspathUriResolver(new ClassloaderClasspathUriResolver());
		this.xtextResourceSet.addLoadOption(XtextResource.OPTION_RESOLVE_ALL, Boolean.TRUE);
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/events.sarl")); //$NON-NLS-1$
		this.xtextResourceSet.createResource(URI.createURI("classpath:/io/sarl/core/bic.sarl")); //$NON-NLS-1$
		return this.xtextResourceSet;
	}
		
	/**
	 * @param s
	 * @param name
	 * @return s
	 */
	public SarlScript mustHavePackage(SarlScript s, String name) {
		assertEquals("Missed package", name, s.getName()); //$NON-NLS-1$
		return s;
	}

	/**
	 * @param s
	 * @return s
	 */
	public SarlScript mustNotHaveImport(SarlScript s) {
		if (s.getImportSection()!=null) {
			assertEquals("Must not have imports", 0, s.getImportSection().getImportDeclarations().size()); //$NON-NLS-1$
		}
		return s;
	}

	/**
	 * @param s
	 * @param numberOfImports
	 * @return s
	 */
	public SarlScript mustHaveImports(SarlScript s, int numberOfImports) {
		assertEquals("Invalid number of imports", numberOfImports, s.getImportSection().getImportDeclarations().size()); //$NON-NLS-1$
		return s;
	}

	/**
	 * @param s
	 * @param numberOfElements
	 * @return s
	 */
	public SarlScript mustHaveTopElements(SarlScript s, int numberOfElements) {
		assertEquals("Invalid number of imports", numberOfElements, s.getElements().size()); //$NON-NLS-1$
		return s;
	}

	/**
	 * @param model
	 * @param index
	 * @param name
	 * @param isStatic
	 * @param hasWildcard
	 * @param isExtension
	 * @return model
	 */
	public SarlScript mustHaveImport(SarlScript model, int index, String name, boolean isStatic, boolean hasWildcard, boolean isExtension) {
		XImportDeclaration d = model.getImportSection().getImportDeclarations().get(index);
		assertNotNull("Null import directive", d); //$NON-NLS-1$
		assertEquals("Not the same imported name", name, d.getImportedName()); //$NON-NLS-1$
		assertEquals("Invalid static modifier", isStatic, d.isStatic()); //$NON-NLS-1$
		assertEquals("Invalid wildcard flag", hasWildcard, d.isWildcard()); //$NON-NLS-1$
		assertEquals("Invalid extension flag", isExtension, d.isExtension()); //$NON-NLS-1$
		return model;
	}

	/**
	 * @param o
	 * @param name
	 * @param superType
	 * @return the agent
	 */
	public Event mustBeEvent(TopElement o, String name, String superType) {
		assertTrue("Not an event type", o instanceof Event); //$NON-NLS-1$
		Event e = (Event)o;
		assertEquals("Not same event name", name, e.getName()); //$NON-NLS-1$
		if (superType!=null) {
			assertEquals("Invalid number of super-types", 1, e.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, e.getSuperTypes().get(0)); //$NON-NLS-1$
		}
		else {
			assertEquals("Invalid number of super-types", 0, e.getSuperTypes().size()); //$NON-NLS-1$
		}
		return e;
	}
	
	/**
	 * @param o
	 * @param name
	 * @param superTypes
	 * @return the agent
	 */
	public Capacity mustBeCapacity(TopElement o, String name, String... superTypes) {
		assertTrue("Not a capacity type", o instanceof Capacity); //$NON-NLS-1$
		Capacity c = (Capacity)o;
		assertEquals("Invalid capacity name", name, c.getName()); //$NON-NLS-1$
		Set<String> set = new TreeSet<>(Arrays.asList(superTypes));
		assertEquals("Invalid number of super-types", superTypes.length, c.getSuperTypes().size()); //$NON-NLS-1$
		for(JvmParameterizedTypeReference ref : c.getSuperTypes()) {
			assertTrue("Unexpected super-type: "+ref.getQualifiedName(), set.remove(ref.getQualifiedName())); //$NON-NLS-1$
		}
		return c;
	}

	/**
	 * @param o
	 * @param name
	 * @param superType
	 * @param capacities
	 * @return the agent
	 */
	public Skill mustBeSkill(TopElement o, String name, String superType, String... capacities) {
		assertTrue("Invalid type of skill", o instanceof Skill); //$NON-NLS-1$
		Skill s = (Skill)o;
		assertEquals("Not same skill name", name, s.getName()); //$NON-NLS-1$
		if (superType!=null) {
			assertEquals("Invalid number of super-types", 1, s.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, s.getSuperTypes().get(0)); //$NON-NLS-1$
		}
		else {
			assertEquals(0, s.getSuperTypes().size());
		}
		Set<String> set = new TreeSet<>(Arrays.asList(capacities));
		assertEquals("Invalid number of capacities", capacities.length, s.getSuperTypes().size()); //$NON-NLS-1$
		for(JvmParameterizedTypeReference ref : s.getSuperTypes()) {
			assertTrue("Unexpected capacity: "+ref.getQualifiedName(), set.remove(ref.getQualifiedName())); //$NON-NLS-1$
		}
		return s;
	}

	/**
	 * @param o
	 * @param name
	 * @param superType
	 * @return the agent
	 */
	public Behavior mustBeBehavior(TopElement o, String name, String superType) {
		assertTrue("Invalid type of behavior", o instanceof Behavior); //$NON-NLS-1$
		Behavior b = (Behavior)o;
		assertEquals("Invalid behavior name", name, b.getName()); //$NON-NLS-1$
		if (superType!=null) {
			assertEquals("Invalid number of super-types", 1, b.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, b.getSuperTypes().get(0)); //$NON-NLS-1$
		}
		else {
			assertEquals("Invalid number of super-types", 0, b.getSuperTypes().size()); //$NON-NLS-1$
		}
		return b;
	}

	/**
	 * @param o
	 * @param name
	 * @param superType
	 * @return the agent
	 */
	public Agent mustBeAgent(TopElement o, String name, String superType) {
		assertTrue("Invalid type of agent", o instanceof Agent); //$NON-NLS-1$
		Agent a = (Agent)o;
		assertEquals("Invalid agent name", name, a.getName()); //$NON-NLS-1$
		if (superType!=null) {
			assertEquals("Invalid number of super-types", 1, a.getSuperTypes().size()); //$NON-NLS-1$
			assertEquals("Invalid super-type", superType, a.getSuperTypes().get(0)); //$NON-NLS-1$
		}
		else {
			assertEquals("Invalid number of super-types", 0, a.getSuperTypes().size()); //$NON-NLS-1$
		}
		return a;
	}

	/**
	 * @param c
	 * @param numberOfFeatures
	 * @return c
	 */
	public <T extends FeatureContainer> T mustHaveFeatures(T c, int numberOfFeatures) {
		assertEquals("Invalid number of features", numberOfFeatures, c.getFeatures().size()); //$NON-NLS-1$
		return c;
	}

	/**
	 * @param o
	 * @param writable
	 * @param name
	 * @param type
	 * @param expression
	 * @return the attribute
	 */
	public Attribute mustBeAttribute(EObject o, boolean writable, String name, String type, boolean expression) {
		assertTrue("Invalid type of attribute", o instanceof Attribute); //$NON-NLS-1$
		Attribute attr = (Attribute)o;
		assertNotNull("Attribute is null", attr); //$NON-NLS-1$
		assertEquals("Invalid name of attribute", name, attr.getName()); //$NON-NLS-1$
		assertEquals("Invalid writable modifier", writable, attr.isWriteable()); //$NON-NLS-1$
		if (type==null) {
			assertNull("Unexpected attribute type", attr.getType()); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting attribute type", attr.getType()); //$NON-NLS-1$
			assertEquals("Unexpected attribute type", type, attr.getType().getQualifiedName()); //$NON-NLS-1$
		}
		assertEquals("Invalid attribute expression", expression, attr.getInitialValue()!=null); //$NON-NLS-1$
		return attr;
	}
	
	/**
	 * @param o
	 * @param name
	 * @param returnType
	 * @param numberOfParameters
	 * @param varargs
	 * @return the action
	 */
	public Action mustBeAction(EObject o, String name, String returnType, int numberOfParameters, boolean varargs) {
		assertTrue("Invalid type of action", o instanceof Action); //$NON-NLS-1$
		Action act = (Action)o;
		ParameterizedFeature pf = act.getSignature();
		assertTrue("Invalid type of action signature", pf instanceof ActionSignature); //$NON-NLS-1$
		ActionSignature sig = (ActionSignature)pf;
		assertEquals("Invalid name of action", name, sig.getName()); //$NON-NLS-1$
		if (returnType==null) {
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

	/**
	 * @param o
	 * @param name
	 * @param returnType
	 * @param numberOfParameters
	 * @param varargs
	 * @return the action
	 */
	public ActionSignature mustBeActionSignature(EObject o, String name, String returnType, int numberOfParameters, boolean varargs) {
		assertTrue("Invalid type of action signature", o instanceof ActionSignature); //$NON-NLS-1$
		ActionSignature sig = (ActionSignature)o;
		assertEquals("Invalid actrion signature name", name, sig.getName()); //$NON-NLS-1$
		if (returnType==null) {
			assertNull("Unexpected return type", sig.getType()); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting return type", sig.getType()); //$NON-NLS-1$
			assertEquals("Invalid return type", returnType, sig.getType().getQualifiedName()); //$NON-NLS-1$
		}
		assertEquals("Invalid number of formal parameters", numberOfParameters, sig.getParams().size()); //$NON-NLS-1$
		assertEquals("Invalid variadic flag", varargs, sig.isVarargs()); //$NON-NLS-1$
		return sig;
	}

	/**
	 * @param o
	 * @param numberOfParameters
	 * @param varargs
	 * @return the action signature
	 */
	public Constructor mustBeConstructor(EObject o, int numberOfParameters, boolean varargs) {
		assertTrue("Invalid constructor type", o instanceof Constructor); //$NON-NLS-1$
		Constructor cons = (Constructor)o;
		assertNotNull("Exepcting body constructor", cons.getBody()); //$NON-NLS-1$
		assertEquals("Invalid number of formal parameters", numberOfParameters, cons.getParams().size()); //$NON-NLS-1$
		assertEquals("Invalid variadic flag", varargs, cons.isVarargs()); //$NON-NLS-1$
		return cons;
	}

	/**
	 * @param o
	 * @param index
	 * @param name
	 * @param type
	 * @param defaultValue
	 * @return 0
	 */
	public <T extends Feature> T mustHaveParameter(T o, int index, String name, String type, boolean defaultValue) {
		ParameterizedFeature pf;
		if (o instanceof Action) {
			pf = ((Action)o).getSignature();
		}
		else {
			pf = (Constructor)o;
		}
		FormalParameter p = pf.getParams().get(index);
		assertNotNull("Expecting formal parameter", p); //$NON-NLS-1$
		assertEquals("Invalid parameter name", name, p.getName()); //$NON-NLS-1$
		if (type==null) {
			assertNull("Unexpected parameter type", p.getParameterType()); //$NON-NLS-1$
			assertNotNull("Expecting default value", p.getDefaultValue()); //$NON-NLS-1$
			assertTrue("Expecting default value", defaultValue); //$NON-NLS-1$
		} else {
			assertNotNull("Expecting parameter type", p.getParameterType()); //$NON-NLS-1$
			assertEquals("Invalid parameter type", type, p.getParameterType().getQualifiedName()); //$NON-NLS-1$
			assertEquals("Invalid default value", defaultValue, p.getDefaultValue()!=null); //$NON-NLS-1$
		}
		return o;
	}

	/**
	 * @param obj
	 * @param func
	 * @return obj
	 */
	public <T> T mustBe(T obj, Functions.Function1<T, Boolean> func){
		assertTrue(func.apply(obj));
		return obj;
	}
	
	private boolean isArray(Object obj) {
		if(obj == null){
			return false;
		}
		return obj.getClass().isArray();
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public <T> T mustBe(T actual, T expected){
		if(isArray(actual) && isArray(expected)){
			assertArrayEquals("not equal", (Object[])expected, (Object[])actual); //$NON-NLS-1$
		}
		else {
			assertEquals("not equal", expected, actual); //$NON-NLS-1$
		}
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XStringLiteral mustBeEqual(XStringLiteral actual, String expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getValue()); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XStringLiteral mustBeEqual(XStringLiteral actual, char expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getValue()); //$NON-NLS-1$
		assertEquals("not equal", 1, actual.getValue().length()); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getValue().charAt(0)); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XBooleanLiteral mustBeEqual(XBooleanLiteral actual, boolean expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.isIsTrue()); //$NON-NLS-1$
		return actual;
	}
	
	private String cleanNumber(String s) {
		if (s==null) return null;
		if (s.startsWith("0x") || s.startsWith("0X")) return s; //$NON-NLS-1$//$NON-NLS-2$
		String literal = s.replace("_", ""); //$NON-NLS-1$//$NON-NLS-2$
		literal = literal.toLowerCase().replaceFirst("l|f|d|(bi)|(bd)$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		return literal;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, Number expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected.toString(), cleanNumber(actual.getValue())); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, byte expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Byte.parseByte(cleanNumber(actual.getValue()))); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, short expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Short.parseShort(cleanNumber(actual.getValue()))); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, int expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getValue()); //$NON-NLS-1$
		String literal = cleanNumber(actual.getValue());
		if (literal.startsWith("0x") || literal.startsWith("0X")) { //$NON-NLS-1$ //$NON-NLS-2$
			assertEquals("not equal", expected, Integer.parseInt(literal.substring(2), 16)); //$NON-NLS-1$
		}
		else {
			assertEquals("not equal", expected, Integer.parseInt(literal)); //$NON-NLS-1$
		}
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, long expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		String literal = cleanNumber(actual.getValue());
		if (literal.startsWith("0x") || literal.startsWith("0X")) { //$NON-NLS-1$ //$NON-NLS-2$
			assertEquals("not equal", expected, Long.parseLong(literal.substring(2), 16)); //$NON-NLS-1$
		}
		else {
			assertEquals("not equal", expected, Long.parseLong(literal)); //$NON-NLS-1$
		}
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, float expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Float.parseFloat(cleanNumber(actual.getValue())), 10e-7); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XNumberLiteral mustBeEqual(XNumberLiteral actual, double expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertEquals("not equal", expected, Double.parseDouble(cleanNumber(actual.getValue())), 10e-7); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XTypeLiteral mustBeEqual(XTypeLiteral actual, String expected){
		assertNotNull("not equal", actual); //$NON-NLS-1$
		assertNotNull("not equal", actual.getType()); //$NON-NLS-1$
		assertEquals("not equal", expected, actual.getType().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XTypeLiteral mustBeEqual(XTypeLiteral actual, Class<?> expected){
		return mustBeEqual(actual, expected.getName());
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XFeatureCall mustBeType(XFeatureCall actual, Class<?> expected){
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertTrue("Feature call is not a type literal", actual.isTypeLiteral()); //$NON-NLS-1$
		assertEquals("Invalid type", expected.getName(), actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XMemberFeatureCall mustCall(XMemberFeatureCall actual, String expected){
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertEquals("Invalid type", expected, actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expected
	 * @return actual
	 */
	public XFeatureCall mustCall(XFeatureCall actual, String expected){
		assertNotNull("null feature call", actual); //$NON-NLS-1$
		assertEquals("Invalid type", expected, actual.getFeature().getQualifiedName()); //$NON-NLS-1$
		return actual;
	}

	/**
	 * @param actual
	 * @param expectedType
	 * @return actual
	 */
	public <T> Class<T> mustBe(Class<T> actual, Class<?> expectedType){
		assertEquals("not equal", expectedType, actual); //$NON-NLS-1$
		return actual;
	}
	
	/**
	 * @param actual
	 * @param expectedType
	 * @return actual
	 */
	public <T> T mustBe(Object actual, Class<T> expectedType){
		String msg = "not equal, expected: "+expectedType.getName()+", actual: "; //$NON-NLS-1$ //$NON-NLS-2$
		if (actual!=null) {
			msg += actual.getClass().getName();
		}
		else {
			msg += actual;
		}
		assertTrue(msg, expectedType.isInstance(actual));
		return expectedType.cast(actual);
	}
	
	/**
	 * @param actual
	 * @param matcher
	 * @return actual
	 */
	public <T> T mustBe(T actual, Matcher<? super T> matcher) {
		if(matcher == null) {
			assertNull("not equal", actual); //$NON-NLS-1$
		}
		else {
			assertTrue("not equal", matcher.matches(actual)); //$NON-NLS-1$
		}
		return actual;
	}
	
	/**
	 * @param actual
	 * @param element
	 * @return actual
	 */
	public <T, I extends Iterable<T>> I mustContain(I actual, T element){
		assertTrue(contains(actual, element));
		return actual;
	}

	/**
	 * @param collection
	 * @param matcher
	 * @return collection
	 */
	public <T, I extends Iterable<T>> I mustContain(I collection, Matcher<? super T> matcher){
		for (T item : collection) {
            if (matcher.matches(item)){
                return collection;
            }
        }
        throw new AssertionFailedError("the collection does not contains an element matching the given critera"); //$NON-NLS-1$
	}
	
	/**
	 * @param actual
	 * @param substring
	 * @return actual
	 */
	public <T extends CharSequence> T mustContain(T actual, CharSequence substring){
		assertTrue(actual.toString().contains(substring));
		return actual;
	}
	
	/**
	 * @param actual
	 * @param result
	 * @return actual
	 */
	public <T> T mustBe(T actual, boolean result) {
		if (actual instanceof Boolean) {
			assertEquals(Boolean.valueOf(result), actual);
		}
		return actual;
	}
	
	/**
	 * @param s
	 * @param substring
	 * @return s
	 */
	public <T extends CharSequence> T mustStartWith(T s, String substring){
		assertTrue(s.toString().startsWith(substring));
		return s;
	} 
	
	/**
	 * @param s
	 * @param substring
	 * @return s
	 */
	public <T extends CharSequence> T mustEndWith(T s, String substring){
		assertTrue(s.toString().endsWith(substring));
		return s;
	}
	
}
