/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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
import static org.jnario.lib.Assert.fail;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstraintOwner;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.ClassloaderClasspathUriResolver;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;
import org.eclipse.xtext.xbase.interpreter.IExpressionInterpreter;
import org.eclipse.xtext.xtype.XImportDeclaration;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;

/** Helper for accessing to the SARL parser.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"static-method", "checkstyle:methodname", "checkstyle:classfanoutcomplexity",
		"checkstyle:methodcount"})
public class SARLParser {

	@Inject
	private ParseHelper<SarlScript> sarlParser;

	@Inject
	private XtextResourceSet xtextResourceSet;

	@Inject
	private ValidationTestHelper validationTestHelper;

	@Inject
	private IExpressionInterpreter interpreter;

	private boolean initial = true;

	/** Replies a list with the given element, if not <code>null</code>.
	 *
	 * @param <T> the type of the elements in the list.
	 * @param element the element.
	 * @return the list with the element, or an empty list if the element is <code>null</code>.
	 */
	public <T> List<T> safeSingleton(T element) {
		if (element == null) {
			return Collections.emptyList();
		}
		return Collections.singletonList(element);
	}

	/** Replies a list with the given element, if not <code>null</code>.
	 *
	 * @param <T> the type of the elements in the list.
	 * @param element the element.
	 * @return the list with the element, or an empty list if the element is <code>null</code>.
	 */
	public <T> List<T> safeList(List<T> element) {
		if (element == null) {
			return Collections.emptyList();
		}
		return element;
	}

	/** Replies a path built from the given elements.
	 *
	 * @param element1 - first mandatory element.
	 * @param elements - the rest of the elements of the path.
	 * @return the path.
	 */
	public String path(String element1, String... elements) {
		StringBuilder b = new StringBuilder();
		if (element1 != null && !element1.isEmpty()) {
			b.append(element1);
		}
		if (elements != null) {
			for (String element : elements) {
				if (element != null && !element.isEmpty()) {
					if (b.length() > 0) {
						b.append(File.separator);
					}
					b.append(element);
				}
			}
		}
		return b.toString();
	}

	/** Parse a SARL code and replies the SARL model.
	 *
	 * <p>This function returns even if the SARL code is
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

	/** Expect an incorrect SARL code.
	 *
	 * <p>Contatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @throws Exception - on parsing error.
	 */
	public void parseWithError(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder buffer = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			buffer.append("\n"); //$NON-NLS-1$
			buffer.append(postfix);
		}
		parseWithError(buffer.toString());
	}

	/** Expect an incorrect SARL code.
	 *
	 * @param text - the SARL code.
	 * @throws Exception - on parsing error.
	 */
	public void parseWithError(CharSequence text) throws Exception {
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

	/** Expect an incorrect SARL code.
	 *
	 * <p>Concatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @throws Exception - on parsing error.
	 */
	public void parseWithError(CharSequence outputText, CharSequence prefix, CharSequence postfix) throws Exception {
		StringBuilder buffer = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			buffer.append(prefix);
			buffer.append("\n"); //$NON-NLS-1$
		}
		buffer.append(outputText);
		if (postfix != null && postfix.length() > 0) {
			buffer.append("\n"); //$NON-NLS-1$
			buffer.append(postfix);
		}
		parseWithError(buffer.toString());
	}

	/** Expect a correct SARL code.
	 *
	 * <p>Concatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parseSuccessfully(CharSequence outputText, CharSequence prefix,
			CharSequence postfix) throws Exception {
		StringBuilder buffer = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			buffer.append(prefix);
			buffer.append("\n"); //$NON-NLS-1$
		}
		buffer.append(outputText);
		if (postfix != null && postfix.length() > 0) {
			buffer.append("\n"); //$NON-NLS-1$
			buffer.append(postfix);
		}
		return parseSuccessfully(buffer.toString());
	}

	/** Expect a correct SARL code.
	 *
	 * @param text - the SARL code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parseSuccessfully(CharSequence text) throws Exception {
		SarlScript model = parse(text);
		this.validationTestHelper.assertNoErrors(model);
		return model;
	}

	/** Expect a correct SARL code.
	 *
	 * <p>Concatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parseSuccessfully(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder buffer = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			buffer.append("\n"); //$NON-NLS-1$
			buffer.append(postfix);
		}
		return parseSuccessfully(buffer.toString());
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
	 * @param script - the SARL script.
	 * @param name - the name of the package.
	 * @return validation status
	 */
	public boolean should_havePackage(SarlScript script, String name) {
		return Objects.equals(script.getPackage(), name);
	}

	/** Ensure that the given SARL script has number of import statements.
	 *
	 * @param obj - the SARL script.
	 * @param numberOfImports - the expected number of imports.
	 * @return validation status
	 */
	public boolean should_haveNbImports(SarlScript obj, int numberOfImports) {
		int nb = 0;
		if (obj != null
				&& obj.getImportSection() != null
				&& obj.getImportSection().getImportDeclarations() != null) {
			nb = obj.getImportSection().getImportDeclarations().size();
		}
		return numberOfImports == nb;
	}

	/** Ensure that the given object has number of elements defined inside.
	 *
	 * @param obj - the object.
	 * @param numberOfElements - the number of top elements defined in the script.
	 * @return the validation status.
	 */
	public boolean should_haveNbElements(EObject obj, int numberOfElements) {
		if (obj instanceof SarlScript) {
			return numberOfElements == ((SarlScript) obj).getXtendTypes().size();
		}
		if (obj instanceof XtendTypeDeclaration) {
			return numberOfElements == ((XtendTypeDeclaration) obj).getMembers().size();
		}
		return false;
	}

	/** Ensure that the given object is an action with the given number of parameters.
	 *
	 * @param obj - the object.
	 * @param numberOfParameters - the number of parameters defined for the action.
	 * @return the validation status.
	 */
	public boolean should_haveNbParameters(EObject obj, int numberOfParameters) {
		int nb = 0;
		if (obj instanceof XtendExecutable) {
			XtendExecutable executable = (XtendExecutable) obj;
			if (executable.getParameters() != null) {
				nb = executable.getParameters().size();
			}
		}
		return nb == numberOfParameters;
	}

	/** Ensure that the given object is a type declaration or function with the given number of type parameters.
	 *
	 * @param obj - the object.
	 * @param numberOfParameters - the number of type parameters defined for the type declaration or function.
	 * @return the validation status.
	 */
	public boolean should_haveNbTypeParameters(EObject obj, int numberOfParameters) {
		int nb = 0;
		if (obj instanceof XtendClass) {
			XtendClass clazz = (XtendClass) obj;
			if (clazz.getTypeParameters() != null) {
				nb = clazz.getTypeParameters().size();
			}
		} else if (obj instanceof XtendInterface) {
			XtendInterface interf = (XtendInterface) obj;
			if (interf.getTypeParameters() != null) {
				nb = interf.getTypeParameters().size();
			}
		} else if (obj instanceof XtendFunction) {
			XtendFunction function = (XtendFunction) obj;
			if (function.getTypeParameters() != null) {
				nb = function.getTypeParameters().size();
			}
		}
		return nb == numberOfParameters;
	}

	/** Ensure that the given SARL script has an import statement of a class.
	 *
	 * @param model - the SARL script.
	 * @param name - the name of the imported type.
	 * @return model
	 */
	public boolean should_importClass(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration declaration : model.getImportSection().getImportDeclarations()) {
			if (declaration != null && Objects.equals(name, declaration.getImportedName())) {
				return !declaration.isStatic() && !declaration.isWildcard() && !declaration.isExtension();
			}
		}
		return false;
	}

	/** Ensure that the given SARL script has an import statement of the classes
	 * in a package.
	 *
	 * @param model - the SARL script.
	 * @param name - the name of the package.
	 * @return model
	 */
	public boolean should_importClassesFrom(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration declaration : model.getImportSection().getImportDeclarations()) {
			if (declaration != null && Objects.equals(name, declaration.getImportedName())) {
				return !declaration.isStatic() && declaration.isWildcard() && !declaration.isExtension();
			}
		}
		return false;
	}

	/** Ensure that the given SARL script has an import statement of the classes
	 * in a package.
	 *
	 * @param model - the SARL script.
	 * @param name - the name of the type.
	 * @return model
	 */
	public boolean should_importMembers(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration declaration : model.getImportSection().getImportDeclarations()) {
			if (declaration != null && Objects.equals(name, declaration.getImportedName())) {
				return declaration.isStatic() && declaration.isWildcard() && !declaration.isExtension();
			}
		}
		return false;
	}

	/** Ensure that the given object is the SARL "event" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the event.
	 * @return the validation status
	 */
	public boolean should_beEvent(EObject obj, String name) {
		if (!(obj instanceof SarlEvent)) {
			return false;
		}
		SarlEvent event = (SarlEvent) obj;
		return Objects.equals(name, event.getName());
	}

	/** Ensure that the given object is the SARL "capacity" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the capacity.
	 * @return the validation status
	 */
	public boolean should_beCapacity(EObject obj, String name) {
		if (!(obj instanceof SarlCapacity)) {
			return false;
		}
		SarlCapacity capacity = (SarlCapacity) obj;
		return Objects.equals(name, capacity.getName());
	}

	/** Ensure that the given object is the SARL "skill" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the skill.
	 * @return the validation status
	 */
	public boolean should_beSkill(EObject obj, String name) {
		if (!(obj instanceof SarlSkill)) {
			return false;
		}
		SarlSkill skill = (SarlSkill) obj;
		return Objects.equals(name, skill.getName());
	}

	/** Ensure that the given object is the SARL "behavior" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the behavior.
	 * @return the validation status
	 */
	public boolean should_beBehavior(EObject obj, String name) {
		if (!(obj instanceof SarlBehavior)) {
			return false;
		}
		SarlBehavior behavior = (SarlBehavior) obj;
		return Objects.equals(name, behavior.getName());
	}

	/** Ensure that the given object is the SARL "agent" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beAgent(EObject obj, String name) {
		if (!(obj instanceof SarlAgent)) {
			return false;
		}
		SarlAgent a = (SarlAgent) obj;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is extending the given type.
	 *
	 * @param obj - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 *     if none.
	 * @return o
	 */
	public boolean should_extend(EObject obj, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (obj instanceof SarlAgent) {
			it = safeSingleton(((SarlAgent) obj).getExtends()).iterator();
		} else if (obj instanceof SarlBehavior) {
			it = safeSingleton(((SarlBehavior) obj).getExtends()).iterator();
		} else if (obj instanceof SarlCapacity) {
			it = safeList(((SarlCapacity) obj).getExtends()).iterator();
		} else if (obj instanceof SarlEvent) {
			it = safeSingleton(((SarlEvent) obj).getExtends()).iterator();
		} else if (obj instanceof SarlSkill) {
			it = safeSingleton(((SarlSkill) obj).getExtends()).iterator();
		} else if (obj instanceof SarlClass) {
			it = safeSingleton(((SarlClass) obj).getExtends()).iterator();
		} else if (obj instanceof SarlInterface) {
			it = safeList(((SarlInterface) obj).getExtends()).iterator();
		} else {
			return false;
		}
		if (superTypes != null) {
			return SpecificationTools.should_iterate(
					it,
					superTypes,
					false);
		}
		return !it.hasNext();
	}

	/** Ensure that the given object is extending the given type.
	 *
	 * @param obj - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 *     if none.
	 * @return o
	 */
	public boolean should_implement(EObject obj, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (obj instanceof SarlSkill) {
			it = safeList(((SarlSkill) obj).getImplements()).iterator();
		} else if (obj instanceof SarlClass) {
			it = safeList(((SarlClass) obj).getImplements()).iterator();
		} else {
			return false;
		}
		if (superTypes != null) {
			return SpecificationTools.should_iterate(
					it,
					superTypes,
					false);
		}
		return !it.hasNext();
	}

	/** Ensure that the given object is implementing the the given number
	 * of types.
	 *
	 * @param obj - the object to test.
	 * @param numberOfImplements - the number of implemented types.
	 * @return o
	 */
	public boolean should_haveNbImplements(EObject obj, int numberOfImplements) {
		int size;
		if (obj instanceof SarlSkill) {
			size = safeList(((SarlSkill) obj).getImplements()).size();
		} else if (obj instanceof SarlClass) {
			size = safeList(((SarlClass) obj).getImplements()).size();
		} else {
			return false;
		}
		return size == numberOfImplements;
	}

	/** Ensure that the given object is the SARL "var" statement.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	public boolean should_beVariable(EObject obj, String name) {
		if (!(obj instanceof SarlField)) {
			return false;
		}
		SarlField attr = (SarlField) obj;
		return Objects.equals(name, attr.getName())
				&& !attr.isFinal();
	}

	/** Ensure that the given object is the SARL "val" statement.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	public boolean should_beValue(EObject obj, String name) {
		if (!(obj instanceof SarlField)) {
			return false;
		}
		SarlField attr = (SarlField) obj;
		return Objects.equals(name, attr.getName())
				&& attr.isFinal();
	}

	/** Ensure that the given object is the SARL "var" or "val" statement
	 * with the given type.
	 *
	 * @param obj - the object to test.
	 * @param type - the expected type name of the attribute, or <code>null</code>.
	 * @return the validation status.
	 */
	public boolean should_haveType(EObject obj, String type) {
		if (obj instanceof SarlField) {
			SarlField attr = (SarlField) obj;
			if (type == null) {
				return attr.getType() == null;
			}
			return attr.getType() != null
					&& Objects.equals(type, attr.getType().getQualifiedName());
		} else if (obj instanceof SarlFormalParameter) {
			SarlFormalParameter param = (SarlFormalParameter) obj;
			if (type == null) {
				return param.getParameterType() == null;
			}
			if (param.getParameterType() != null) {
				String parameterType = param.getParameterType().getQualifiedName();
				return Objects.equals(type, parameterType);
			}
		}
		return false;
	}

	/** Ensure that the given object is the SARL "def" statement (with body).
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	public boolean should_beAction(EObject obj, String name) {
		if (!(obj instanceof SarlAction)) {
			return false;
		}
		SarlAction act = (SarlAction) obj;
		return Objects.equals(name, act.getName()) && act.getExpression() != null;
	}

	/** Ensure that the given object is the SARL "def" statement with the given modifiers.
	 *
	 * @param obj - the object to test.
	 * @param modifiers - the string that describes the modifier, or a collection of strings.
	 * @return validation status
	 */
	@SuppressWarnings("unchecked")
	public boolean should_haveModifiers(EObject obj, Object modifiers) {
		if ((!(obj instanceof XtendMember)) || modifiers == null) {
			return false;
		}
		XtendMember member = (XtendMember) obj;
		EList<String> currentModifiers = member.getModifiers();
		Iterable<String> it;
		if (modifiers instanceof String[]) {
			it = Arrays.asList((String[]) modifiers);
		} else if (modifiers instanceof Iterable) {
			it = (Iterable<String>) modifiers;
		} else {
			return currentModifiers.contains(modifiers.toString());
		}

		for (String expected : it) {
			if (!currentModifiers.contains(expected)) {
				return false;
			}
		}
		return true;
	}

	/** Ensure that the given object is the SARL "def" statement (without body).
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	public boolean should_beActionSignature(EObject obj, String name) {
		if (!(obj instanceof SarlAction)) {
			return false;
		}
		SarlAction sig = (SarlAction) obj;
		return Objects.equals(name, sig.getName()) && sig.getExpression() == null;
	}

	/** Ensure that the given object is the SARL "on" statement.
	 *
	 * @param obj - the object to test.
	 * @param event - the name of expected event.
	 * @return validation status
	 */
	public boolean should_beBehaviorUnit(EObject obj, String event) {
		if (!(obj instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) obj;
		return Objects.equals(event, bu.getName().getQualifiedName());
	}

	/** Ensure that the given object is the SARL "uses" statement.
	 *
	 * @param obj - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	public boolean should_beCapacityUse(EObject obj, Object capacities) {
		if (!(obj instanceof SarlCapacityUses)) {
			return false;
		}
		SarlCapacityUses uses = (SarlCapacityUses) obj;
		if (uses.getCapacities() != null) {
			return SpecificationTools.should_iterate(
					uses.getCapacities().iterator(),
					capacities,
					false);
		}
		return false;
	}

	/** Ensure that the given object is the SARL "requires" statement.
	 *
	 * @param obj - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	public boolean should_beCapacityRequirement(EObject obj, Object capacities) {
		if (!(obj instanceof SarlRequiredCapacity)) {
			return false;
		}
		SarlRequiredCapacity reqs = (SarlRequiredCapacity) obj;
		if (reqs.getCapacities() != null) {
			return SpecificationTools.should_iterate(
					reqs.getCapacities().iterator(),
					capacities);
		}
		return false;
	}

	/** Ensure that the given object is a behavior unit with or
	 * with a guard.
	 *
	 * @param obj - the object to test.
	 * @param guard - the expected guard.
	 * @return validation status
	 */
	public boolean should_beGuardedWith(EObject obj, String guard) {
		if (!(obj instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) obj;
		XExpression actualGuard = bu.getGuard();
		if (guard == null) {
			return actualGuard == null;
		}
		if (SpecificationTools.should_beLiteral(actualGuard, guard)) {
			return true;
		}
		String code;
		if (actualGuard != null) {
			ICompositeNode node = NodeModelUtils.getNode(actualGuard);
			if (node == null) {
				code = ""; //$NON-NLS-1$
			} else {
				code = node.getText();
			}
		} else {
			code = ""; //$NON-NLS-1$
		}
		return Objects.equals(guard, code.trim());
	}

	/** Ensure that the given object is the SARL "new" statement (with body).
	 *
	 * @param obj - the object to test.
	 * @param something - not used.
	 * @return validation status
	 */
	public boolean should_beConstructor(EObject obj, Object something) {
		if (!(obj instanceof SarlConstructor)) {
			return false;
		}
		return true;
	}

	/** Ensure that the given object is the SARL "def" statement
	 * that is variadic or not.
	 *
	 * @param obj - the object to test.
	 * @param isVariadic - the expected variadic flag
	 * @return validation status
	 */
	public boolean should_beVariadic(EObject obj, boolean isVariadic) {
		XtendExecutable executable;
		if (obj instanceof XtendExecutable) {
			executable = (XtendExecutable) obj;
		} else {
			return false;
		}
		boolean i;
		if (executable.getParameters() == null || executable.getParameters().isEmpty()) {
			i = false;
		} else {
			i = executable.getParameters().get(executable.getParameters().size() - 1).isVarArg();
		}
		return i == isVariadic;
	}

	/** Ensure that the given object is the SARL "def" statement (with body)
	 * that is returning the given type.
	 *
	 * @param obj - the object to test.
	 * @param returnType - the name of the expected return type.
	 * @return the validation status
	 */
	public boolean should_reply(EObject obj, String returnType) {
		JvmTypeReference rtype;
		if (obj instanceof SarlAction) {
			rtype = ((SarlAction) obj).getReturnType();
		} else {
			return false;
		}
		if (returnType == null) {
			return rtype == null;
		}
		if (rtype != null) {
			String name = rtype.getQualifiedName();
			return Objects.equals(returnType, name);
		}
		return false;
	}

	/**  Ensure that the given object is the SARL "def" statement
	 * that is throwing exceptions.
	 *
	 * @param object - the object to test.
	 * @param exceptions - the list of exception, or the single exception.
	 * @return the validation status.
	 */
	public boolean should_throwException(EObject object, Object exceptions) {
		List<JvmTypeReference> list;
		if (object instanceof SarlAction) {
			list = ((SarlAction) object).getExceptions();
		} else {
			return false;
		}
		Set<String> expectedExceptions = new TreeSet<>();
		if (exceptions instanceof Iterable<?>) {
			for (Object obj : (Iterable<?>) exceptions) {
				if (obj != null) {
					String name = obj.toString();
					if (!Strings.isNullOrEmpty(name)) {
						expectedExceptions.add(name);
					}
				}
			}
		} else if (exceptions != null) {
			String name = exceptions.toString();
			if (!Strings.isNullOrEmpty(name)) {
				expectedExceptions.add(name);
			}
		}
		if (expectedExceptions.size() != list.size()) {
			return false;
		}
		for (JvmTypeReference ref : list) {
			if (!expectedExceptions.remove(ref.getQualifiedName())) {
				return false;
			}
		}
		return expectedExceptions.isEmpty();
	}

	/**  Ensure that the given object is the SARL "def" statement
	 * that is firing events.
	 *
	 * @param object - the object to test.
	 * @param events - the list of events, or the single event.
	 * @return the validation status.
	 */
	public static boolean should_fireEvents(EObject object, Object events) {
		List<JvmTypeReference> list;
		if (object instanceof SarlAction) {
			list = ((SarlAction) object).getFiredEvents();
		} else {
			return false;
		}
		Set<String> expectedEvents = new TreeSet<>();
		if (events instanceof Iterable<?>) {
			for (Object obj : (Iterable<?>) events) {
				if (obj != null) {
					String name = obj.toString();
					if (!Strings.isNullOrEmpty(name)) {
						expectedEvents.add(name);
					}
				}
			}
		} else if (events != null) {
			String name = events.toString();
			if (!Strings.isNullOrEmpty(name)) {
				expectedEvents.add(name);
			}
		}
		if (expectedEvents.size() != list.size()) {
			return false;
		}
		for (JvmTypeReference ref : list) {
			if (!expectedEvents.remove(ref.getQualifiedName())) {
				return false;
			}
		}
		return expectedEvents.isEmpty();
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param obj - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	public boolean should_beParameter(EObject obj, String name) {
		if (!(obj instanceof SarlFormalParameter)) {
			return false;
		}
		SarlFormalParameter parameter = (SarlFormalParameter) obj;
		return Objects.equals(name, parameter.getName());
	}

	/** Ensure that the given feature has a type parameter.
	 *
	 * @param obj - the feature to test.
	 * @param name - the expected name of the type parameter.
	 * @return the validation status
	 */
	public boolean should_beTypeParameter(EObject obj, String name) {
		if (!(obj instanceof JvmTypeParameter)) {
			return false;
		}
		JvmTypeParameter parameter = (JvmTypeParameter) obj;
		return Objects.equals(name, parameter.getName());
	}

	/** Ensure that the given feature has a constraint.
	 *
	 * @param obj - the feature to test.
	 * @param constraint - the expected constraint.
	 * @return the validation status
	 */
	public boolean should_beContrainedWith(EObject obj, String constraint) {
		if (!(obj instanceof JvmConstraintOwner)) {
			return false;
		}
		JvmConstraintOwner owner = (JvmConstraintOwner) obj;
		if (Strings.isNullOrEmpty(constraint)) {
			return owner.getConstraints().isEmpty();
		} else if (owner.getConstraints().size() != 1) {
			return false;
		}
		return Objects.equals(constraint, owner.getConstraints().get(0).getQualifiedName());
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param obj - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	public boolean should_haveDefaultValue(EObject obj, Object name) {
		if (!(obj instanceof SarlFormalParameter)) {
			return false;
		}
		SarlFormalParameter parameter = (SarlFormalParameter) obj;
		if (name == null) {
			return parameter.getDefaultValue() == null;
		}
		return SpecificationTools.should_beLiteral(parameter.getDefaultValue(), name);
	}

	/** Ensure that the given feature is an attribute with an initial value.
	 *
	 * @param obj - the feature to test.
	 * @param initialValue - the expected literal for the initial value.
	 * @return the validation status
	 */
	public boolean should_haveInitialValue(EObject obj, Object initialValue) {
		if (!(obj instanceof SarlField)) {
			return false;
		}
		SarlField field = (SarlField) obj;
		if (initialValue == null) {
			return field.getInitialValue() == null;
		}
		XExpression expr = field.getInitialValue();
		if ((expr instanceof XFeatureCall) || (expr instanceof XMemberFeatureCall)
				|| (expr instanceof XConstructorCall)) {
			return should_call(expr, initialValue.toString());
		}
		if (expr instanceof XCastedExpression) {
			XCastedExpression castedExpr = (XCastedExpression) expr;
			if (Objects.equals(
					castedExpr.getType().getQualifiedName(),
					initialValue.toString())) {
				return true;
			}
			return SpecificationTools.should_beLiteral(castedExpr.getTarget(), initialValue);
		}
		return SpecificationTools.should_beLiteral(field.getInitialValue(), initialValue);
	}

	/** Ensure that the given feature call is calling the feature with
	 * the given name.
	 *
	 * @param actual - the feature call to test.
	 * @param expected - the expected name of the feature.
	 * @return the validation status
	 */
	public boolean should_call(EObject actual, String expected) {
		if (actual instanceof XMemberFeatureCall) {
			XMemberFeatureCall call = (XMemberFeatureCall) actual;
			return Objects.equals(expected, call.getFeature().getQualifiedName());
		}
		if (actual instanceof XFeatureCall) {
			XFeatureCall call = (XFeatureCall) actual;
			return Objects.equals(expected, call.getFeature().getQualifiedName());
		}
		if (actual instanceof XConstructorCall) {
			XConstructorCall c = (XConstructorCall) actual;
			String consName = c.getConstructor().getQualifiedName()
					+ "." + c.getConstructor().getSimpleName(); //$NON-NLS-1$
			return Objects.equals(expected, consName);
		}
		return false;
	}

	/** Parse a Xbase expression.
	 *
	 * @param expression - the expression to parse.
	 * @return the XExpression that is corresponding to the given expression.
	 * @throws Exception if the expression cannot be parsed.
	 */
	public XExpression expression(String expression) throws Exception {
		return expression(expression, true);
	}

	/** Parse a Xbase expression.
	 *
	 * @param expression - the expression to parse.
	 * @param resolve - <code>true</code> if the expression must have no error, <code>false</code>
	 *     to not care.
	 * @return the XExpression that is corresponding to the given expression.
	 * @throws Exception if the expression cannot be parsed.
	 */
	public XExpression expression(String expression, boolean resolve) throws Exception {
		String code = "def ____TeStInG_FuNcTiOn() : Object {\n" //$NON-NLS-1$
				+ expression
				+ "\n}"; //$NON-NLS-1$
		SarlAction action = (SarlAction) agentCode("AgentXXXXX", code, resolve).get(0); //$NON-NLS-1$
		XBlockExpression block = (XBlockExpression) action.getExpression();
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
	 *     to not care.
	 * @return the statements in the agent definition.
	 * @throws Exception if the code cannot be parsed.
	 */
	public List<? extends EObject> agentCode(String agentTypeName, String code, boolean resolve) throws Exception {
		String fullCode = "agent " + agentTypeName //$NON-NLS-1$
				+ " {\n" + code //$NON-NLS-1$
				+ "\n}\n"; //$NON-NLS-1$
		SarlScript script = parse(fullCode);
		if (resolve) {
			this.validationTestHelper.assertNoErrors(script);
		}
		SarlAgent agent = (SarlAgent) script.getXtendTypes().get(0);
		return agent.getMembers();
	}

	/** Evaluate an expression and reply the result.
	 *
	 * @param <T> - the expected type of the result.
	 * @param expression - the expression to evaluate.
	 * @param resultType - the expected type of the result.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or not of the given type.
	 */
	public <T> T to(String expression, Class<T> resultType) throws Exception {
		XExpression expr = expression(expression);
		IEvaluationResult result = this.interpreter.evaluate(expr);
		if (result == null) {
			throw new RuntimeException("cannot evaluate"); //$NON-NLS-1$
		}
		Throwable exception = result.getException();
		if (exception != null) {
			if (exception instanceof Exception) {
				throw (Exception) exception;
			} else if (exception instanceof Error) {
				throw (Error) exception;
			}
			throw new RuntimeException(exception);
		}
		Object value = result.getResult();
		if (value == null) {
			return resultType.cast(null);
		}
		if (resultType.isInstance(value)) {
			return resultType.cast(value);
		}
		fail("Invalid type. Expected: " //$NON-NLS-1$
				+ resultType.getName() + ", but was: " //$NON-NLS-1$
				+ value.getClass().getName());
		return resultType.cast(null);
	}

	/** Evaluate a byte expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public byte toByte(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.byteValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a short integer expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public short toShort(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.shortValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate an integer expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public int toInt(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.intValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a long integer expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public long toLong(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.longValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a single-precision floating point expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public float toFloat(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.floatValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a double-precision floating point expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public double toDouble(String expression) throws Exception {
		Number obj = to(expression, Number.class);
		if (obj != null) {
			return obj.doubleValue();
		}
		fail("Illegal number"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a character expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a character.
	 */
	public char toChar(String expression) throws Exception {
		Character obj = to(expression, Character.class);
		if (obj != null) {
			return obj.charValue();
		}
		fail("Illegal character"); //$NON-NLS-1$
		return 0;
	}

	/** Evaluate a boolean expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed or is not a boolean.
	 */
	public boolean toBool(String expression) throws Exception {
		Boolean obj = to(expression, Boolean.class);
		if (obj != null) {
			return obj.booleanValue();
		}
		fail("Illegal boolean value"); //$NON-NLS-1$
		return false;
	}

	/** Evaluate a boolean expression and reply the result.
	 *
	 * @param expression - the expression to evaluate.
	 * @return the result of the evaluation.
	 * @throws Exception if the expression cannot be parsed.
	 */
	public String toStr(String expression) throws Exception {
		Object obj = to(expression, Object.class);
		if (obj != null) {
			return obj.toString();
		}
		return null;
	}

	/** Ensure that the given object is the SARL "class" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beClass(EObject obj, String name) {
		if (!(obj instanceof SarlClass)) {
			return false;
		}
		SarlClass a = (SarlClass) obj;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "interface" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beInterface(EObject obj, String name) {
		if (!(obj instanceof SarlInterface)) {
			return false;
		}
		SarlInterface a = (SarlInterface) obj;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beEnumeration(EObject obj, String name) {
		if (!(obj instanceof SarlEnumeration)) {
			return false;
		}
		SarlEnumeration a = (SarlEnumeration) obj;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param obj - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beAnnotation(EObject obj, String name) {
		if (!(obj instanceof SarlAnnotationType)) {
			return false;
		}
		SarlAnnotationType a = (SarlAnnotationType) obj;
		return Objects.equals(name, a.getName());
	}

}
