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

import com.google.common.base.Predicate;
import com.google.inject.Inject;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
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
@SuppressWarnings({"static-method", "checkstyle:classfanoutcomplexity", "checkstyle:methodcount"})
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
	 * @param <T> - the type of the singleton.
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
	 * @param <T> - the type of the list elements.
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
	 * <p>Concatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public SarlScript parseSuccessfully(CharSequence outputText, CharSequence prefix, CharSequence postfix) throws Exception {
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
		return parseSuccessfully(b.toString());
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
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		return parseSuccessfully(b.toString());
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
	 * <p>Contatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @throws Exception - on parsing error.
	 */
	public void parseWithError(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		parseWithError(b.toString());
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
		parseWithError(b.toString());
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
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_havePackage(SarlScript script, String name) {
		return Objects.equals(script.getPackage(), name);
	}

	/** Ensure that the given SARL script has number of import statements.
	 *
	 * @param script - the SARL script.
	 * @param numberOfImports - the expected number of imports.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveNbImports(SarlScript script, int numberOfImports) {
		int nb = 0;
		if (script != null
				&& script.getImportSection() != null
				&& script.getImportSection().getImportDeclarations() != null) {
			nb = script.getImportSection().getImportDeclarations().size();
		}
		return numberOfImports == nb;
	}

	/** Ensure that the given object has number of elements defined inside.
	 *
	 * @param object - the object.
	 * @param numberOfElements - the number of top elements defined in the script.
	 * @return the validation status.
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveNbElements(EObject object, int numberOfElements) {
		if (object instanceof SarlScript) {
			return numberOfElements == ((SarlScript) object).getXtendTypes().size();
		}
		if (object instanceof XtendTypeDeclaration) {
			return numberOfElements == ((XtendTypeDeclaration) object).getMembers().size();
		}
		return false;
	}

	/** Ensure that the given object is an action with the given number of parameters.
	 *
	 * @param object - the object.
	 * @param numberOfParameters - the number of parameters defined for the action.
	 * @return the validation status.
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveNbParameters(EObject object, int numberOfParameters) {
		int nb = 0;
		EObject obj = object;
		if (obj instanceof XtendExecutable) {
			XtendExecutable executable = (XtendExecutable) obj;
			if (executable.getParameters() != null) {
				nb = executable.getParameters().size();
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
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_importClass(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
			if (d != null && Objects.equals(name, d.getImportedName())) {
				return !d.isStatic() && !d.isWildcard() && !d.isExtension();
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
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_importClassesFrom(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
			if (d != null && Objects.equals(name, d.getImportedName())) {
				return !d.isStatic() && d.isWildcard() && !d.isExtension();
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
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_importMembers(SarlScript model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for (XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
			if (d != null && Objects.equals(name, d.getImportedName())) {
				return d.isStatic() && d.isWildcard() && !d.isExtension();
			}
		}
		return false;
	}

	/** Ensure that the given object is the SARL "event" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the event.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beEvent(EObject object, String name) {
		if (!(object instanceof SarlEvent)) {
			return false;
		}
		SarlEvent event = (SarlEvent) object;
		return Objects.equals(name, event.getName());
	}

	/** Ensure that the given object is the SARL "capacity" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the capacity.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beCapacity(EObject object, String name) {
		if (!(object instanceof SarlCapacity)) {
			return false;
		}
		SarlCapacity c = (SarlCapacity) object;
		return Objects.equals(name, c.getName());
	}

	/** Ensure that the given object is the SARL "skill" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the skill.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beSkill(EObject object, String name) {
		if (!(object instanceof SarlSkill)) {
			return false;
		}
		SarlSkill skill = (SarlSkill) object;
		return Objects.equals(name, skill.getName());
	}

	/** Ensure that the given object is the SARL "behavior" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the behavior.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beBehavior(EObject object, String name) {
		if (!(object instanceof SarlBehavior)) {
			return false;
		}
		SarlBehavior b = (SarlBehavior) object;
		return Objects.equals(name, b.getName());
	}

	/** Ensure that the given object is the SARL "agent" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beAgent(EObject object, String name) {
		if (!(object instanceof SarlAgent)) {
			return false;
		}
		SarlAgent a = (SarlAgent) object;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is extending the given type.
	 *
	 * @param object - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 *     if none.
	 * @return o
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_extend(EObject object, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (object instanceof SarlAgent) {
			it = safeSingleton(((SarlAgent) object).getExtends()).iterator();
		} else if (object instanceof SarlBehavior) {
			it = safeSingleton(((SarlBehavior) object).getExtends()).iterator();
		} else if (object instanceof SarlCapacity) {
			it = safeList(((SarlCapacity) object).getExtends()).iterator();
		} else if (object instanceof SarlEvent) {
			it = safeSingleton(((SarlEvent) object).getExtends()).iterator();
		} else if (object instanceof SarlSkill) {
			it = safeSingleton(((SarlSkill) object).getExtends()).iterator();
		} else if (object instanceof SarlClass) {
			it = safeSingleton(((SarlClass) object).getExtends()).iterator();
		} else if (object instanceof SarlInterface) {
			it = safeList(((SarlInterface) object).getExtends()).iterator();
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
	 * @param object - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 *     if none.
	 * @return o
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_implement(EObject object, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (object instanceof SarlSkill) {
			it = safeList(((SarlSkill) object).getImplements()).iterator();
		} else if (object instanceof SarlClass) {
			it = safeList(((SarlClass) object).getImplements()).iterator();
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
	 * @param object - the object to test.
	 * @param numberOfImplements - the number of implemented types.
	 * @return o
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveNbImplements(EObject object, int numberOfImplements) {
		int size;
		if (object instanceof SarlSkill) {
			size = safeList(((SarlSkill) object).getImplements()).size();
		} else if (object instanceof SarlClass) {
			size = safeList(((SarlClass) object).getImplements()).size();
		} else {
			return false;
		}
		return size == numberOfImplements;
	}

	/** Ensure that the given object is the SARL "var" statement.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beVariable(EObject object, String name) {
		if (!(object instanceof SarlField)) {
			return false;
		}
		SarlField attr = (SarlField) object;
		return Objects.equals(name, attr.getName())
				&& !attr.isFinal();
	}

	/** Ensure that the given object is the SARL "val" statement.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beValue(EObject object, String name) {
		if (!(object instanceof SarlField)) {
			return false;
		}
		SarlField attr = (SarlField) object;
		return Objects.equals(name, attr.getName())
				&& attr.isFinal();
	}

	/** Ensure that the given object is the SARL "var" or "val" statement
	 * with the given type.
	 *
	 * @param object - the object to test.
	 * @param type - the expected type name of the attribute, or <code>null</code>.
	 * @return the validation status.
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveType(EObject object, String type) {
		if (object instanceof SarlField) {
			SarlField attr = (SarlField) object;
			if (type == null) {
				return attr.getType() == null;
			}
			return attr.getType() != null
					&& Objects.equals(type, attr.getType().getQualifiedName());
		} else if (object instanceof SarlFormalParameter) {
			SarlFormalParameter param = (SarlFormalParameter) object;
			if (type == null) {
				return param.getParameterType() == null;
			}
			return param.getParameterType() != null
					&& Objects.equals(type, param.getParameterType().getQualifiedName());
		}
		return false;
	}

	/** Ensure that the given object is the SARL "def" statement (with body).
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beAction(EObject object, String name) {
		if (!(object instanceof SarlAction)) {
			return false;
		}
		SarlAction act = (SarlAction) object;
		return Objects.equals(name, act.getName()) && act.getExpression() != null;
	}

	/** Ensure that the given object is the SARL "def" statement with the given modifiers.
	 *
	 * @param object - the object to test.
	 * @param modifiers - the string that describes the modifier, or a collection of strings.
	 * @return validation status
	 */
	@SuppressWarnings({"unchecked", "checkstyle:methodname"})
	public boolean should_haveModifiers(EObject object, Object modifiers) {
		if ((!(object instanceof XtendMember)) || modifiers == null) {
			return false;
		}
		XtendMember member = (XtendMember) object;
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
	 * @param object - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beActionSignature(EObject object, String name) {
		if (!(object instanceof SarlAction)) {
			return false;
		}
		SarlAction sig = (SarlAction) object;
		return Objects.equals(name, sig.getName()) && sig.getExpression() == null;
	}

	/** Ensure that the given object is the SARL "on" statement.
	 *
	 * @param object - the object to test.
	 * @param event - the name of expected event.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beBehaviorUnit(EObject object, String event) {
		if (!(object instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) object;
		return Objects.equals(event, bu.getName().getQualifiedName());
	}

	/** Ensure that the given object is the SARL "uses" statement.
	 *
	 * @param object - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beCapacityUse(EObject object, Object capacities) {
		if (!(object instanceof SarlCapacityUses)) {
			return false;
		}
		SarlCapacityUses uses = (SarlCapacityUses) object;
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
	 * @param object - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beCapacityRequirement(EObject object, Object capacities) {
		if (!(object instanceof SarlRequiredCapacity)) {
			return false;
		}
		SarlRequiredCapacity reqs = (SarlRequiredCapacity) object;
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
	 * @param object - the object to test.
	 * @param guard - the expected guard.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beGuardedWith(EObject object, String guard) {
		if (!(object instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) object;
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
	 * @param object - the object to test.
	 * @param something - not used.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beConstructor(EObject object, Object something) {
		if (!(object instanceof SarlConstructor)) {
			return false;
		}
		return true;
	}

	/** Ensure that the given object is the SARL "def" statement
	 * that is variadic or not.
	 *
	 * @param object - the object to test.
	 * @param isVariadic - the expected variadic flag
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beVariadic(EObject object, boolean isVariadic) {
		XtendExecutable executable;
		if (object instanceof XtendExecutable) {
			executable = (XtendExecutable) object;
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
	 * @param object - the object to test.
	 * @param returnType - the name of the expected return type.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_reply(EObject object, String returnType) {
		JvmTypeReference typeReference;
		if (object instanceof SarlAction) {
			typeReference = ((SarlAction) object).getReturnType();
		} else {
			return false;
		}
		if (returnType == null) {
			return typeReference == null;
		}
		return typeReference != null
				&& Objects.equals(returnType, typeReference.getQualifiedName());
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param object - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beParameter(EObject object, String name) {
		if (!(object instanceof SarlFormalParameter)) {
			return false;
		}
		SarlFormalParameter parameter = (SarlFormalParameter) object;
		return Objects.equals(name, parameter.getName());
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param object - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveDefaultValue(EObject object, Object name) {
		if (!(object instanceof SarlFormalParameter)) {
			return false;
		}
		SarlFormalParameter parameter = (SarlFormalParameter) object;
		if (name == null) {
			return parameter.getDefaultValue() == null;
		}
		return SpecificationTools.should_beLiteral(parameter.getDefaultValue(), name);
	}

	/** Ensure that the given feature is an attribute with an initial value.
	 *
	 * @param object - the feature to test.
	 * @param initialValue - the expected literal for the initial value.
	 * @return the validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_haveInitialValue(EObject object, Object initialValue) {
		if (!(object instanceof SarlField)) {
			return false;
		}
		SarlField field = (SarlField) object;
		if (initialValue == null) {
			return field.getInitialValue() == null;
		}
		XExpression expr = field.getInitialValue();
		if ((expr instanceof XFeatureCall) || (expr instanceof XMemberFeatureCall)
				|| (expr instanceof XConstructorCall)) {
			return should_call(expr, initialValue.toString());
		}
		if (expr instanceof XCastedExpression) {
			XCastedExpression castedExpression = (XCastedExpression) expr;
			if (Objects.equals(
					castedExpression.getType().getQualifiedName(),
					initialValue.toString())) {
				return true;
			}
			return SpecificationTools.should_beLiteral(castedExpression.getTarget(), initialValue);
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
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_call(EObject actual, String expected) {
		if (actual instanceof XMemberFeatureCall) {
			XMemberFeatureCall c = (XMemberFeatureCall) actual;
			return Objects.equals(expected, c.getFeature().getQualifiedName());
		}
		if (actual instanceof XFeatureCall) {
			XFeatureCall c = (XFeatureCall) actual;
			return Objects.equals(expected, c.getFeature().getQualifiedName());
		}
		if (actual instanceof XConstructorCall) {
			XConstructorCall consCall = (XConstructorCall) actual;
			String consName = consCall.getConstructor().getQualifiedName()
					+ "." + consCall.getConstructor().getSimpleName(); //$NON-NLS-1$
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
		IEvaluationResult evalResult = this.interpreter.evaluate(expr);
		if (evalResult == null) {
			throw new RuntimeException("cannot evaluate"); //$NON-NLS-1$
		}
		Throwable exception = evalResult.getException();
		if (exception != null) {
			if (exception instanceof Exception) {
				throw (Exception) exception;
			} else if (exception instanceof Error) {
				throw (Error) exception;
			}
			throw new RuntimeException(exception);
		}
		Object v = evalResult.getResult();
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
	 */
	public byte toByte(String expression) throws Exception {
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.byteValue();
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
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.shortValue();
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
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.intValue();
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
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.longValue();
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
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.floatValue();
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
		Number number = to(expression, Number.class);
		if (number != null) {
			return number.doubleValue();
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
		Character character = to(expression, Character.class);
		if (character != null) {
			return character.charValue();
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
		Boolean bool = to(expression, Boolean.class);
		if (bool != null) {
			return bool.booleanValue();
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
		Object object = to(expression, Object.class);
		if (object != null) {
			return object.toString();
		}
		return null;
	}

	/** Ensure that the given object is the SARL "class" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beClass(EObject object, String name) {
		if (!(object instanceof SarlClass)) {
			return false;
		}
		SarlClass a = (SarlClass) object;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "interface" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beInterface(EObject object, String name) {
		if (!(object instanceof SarlInterface)) {
			return false;
		}
		SarlInterface a = (SarlInterface) object;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beEnumeration(EObject object, String name) {
		if (!(object instanceof SarlEnumeration)) {
			return false;
		}
		SarlEnumeration a = (SarlEnumeration) object;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param object - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	@SuppressWarnings("checkstyle:methodname")
	public boolean should_beAnnotation(EObject object, String name) {
		if (!(object instanceof SarlAnnotationType)) {
			return false;
		}
		SarlAnnotationType a = (SarlAnnotationType) object;
		return Objects.equals(name, a.getName());
	}

}
