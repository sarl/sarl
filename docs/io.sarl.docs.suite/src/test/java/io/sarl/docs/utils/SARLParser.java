/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;

import java.io.File;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
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

	@Inject
	private ParseHelper<XtendFile> sarlParser;
	@Inject
	private XtextResourceSet xtextResourceSet;
	@Inject
	private ValidationTestHelper validationTestHelper;
	@Inject
	private IExpressionInterpreter interpreter;

	private boolean initial = true;

	/** Replies a list with the given element, if not <code>null</code>.
	 *
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
	 * <p>
	 * This function returns even if the SARL code is
	 * incorrect.
	 *
	 * @param text - the code to parse.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public XtendFile parse(CharSequence text) throws Exception {
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
	public XtendFile parseSuccessfully(CharSequence text) throws Exception {
		XtendFile model = parse(text);
		this.validationTestHelper.assertNoErrors(model);
		return model;
	}

	/** Expect an incorrect SARL code.
	 *
	 * @param text - the SARL code.
	 * @throws Exception - on parsing error.
	 */
	public void parseWithError(CharSequence text) throws Exception {
		XtendFile model = parse(text);

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
	 * Concatenation of the two parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public XtendFile parseSuccessfully(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		return parseSuccessfully(b.toString());
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
	public void parseWithError(CharSequence outputText, CharSequence postfix) throws Exception {
		StringBuilder b = new StringBuilder(outputText);
		if (postfix != null && postfix.length() > 0) {
			b.append("\n"); //$NON-NLS-1$
			b.append(postfix);
		}
		parseWithError(b.toString());
	}

	/** Expect a correct SARL code.
	 * <p>
	 * Concatenation of the three parameters and parse the result
	 * as a SARL code.
	 *
	 * @param outputText - main part of the code.
	 * @param prefix - the prefix code.
	 * @param postfix - the postfix code.
	 * @return the SARL model.
	 * @throws Exception - on parsing error.
	 */
	public XtendFile parseSuccessfully(CharSequence outputText, CharSequence prefix, CharSequence postfix) throws Exception {
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

	/** Expect an incorrect SARL code.
	 * <p>
	 * Concatenation of the three parameters and parse the result
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
	 * @param s - the SARL script.
	 * @param name - the name of the package.
	 * @return validation status
	 */
	public boolean should_havePackage(XtendFile s, String name) {
		return Objects.equals(s.getPackage(), name);
	}

	/** Ensure that the given SARL script has number of import statements.
	 *
	 * @param s - the SARL script.
	 * @param numberOfImports - the expected number of imports.
	 * @return validation status
	 */
	public boolean should_haveNbImports(XtendFile s, int numberOfImports) {
		int nb = 0;
		if (s != null
				&& s.getImportSection() != null
				&& s.getImportSection().getImportDeclarations() != null) {
			nb = s.getImportSection().getImportDeclarations().size();
		}
		return numberOfImports == nb;
	}

	/** Ensure that the given object has number of elements defined inside.
	 *
	 * @param s - the object.
	 * @param numberOfElements - the number of top elements defined in the script.
	 * @return the validation status.
	 */
	public boolean should_haveNbElements(EObject s, int numberOfElements) {
		if (s instanceof XtendFile) {
			return numberOfElements == ((XtendFile) s).getXtendTypes().size();
		}
		if (s instanceof XtendTypeDeclaration) {
			return numberOfElements == ((XtendTypeDeclaration) s).getMembers().size();
		}
		return false;
	}

	/** Ensure that the given object is an action with the given number of parameters.
	 *
	 * @param s - the object.
	 * @param numberOfParameters - the number of parameters defined for the action.
	 * @return the validation status.
	 */
	public boolean should_haveNbParameters(EObject s, int numberOfParameters) {
		int nb = 0;
		EObject obj = s;
		if (obj instanceof XtendExecutable) {
			XtendExecutable f = (XtendExecutable) obj;
			if (f.getParameters() != null) {
				nb = f.getParameters().size();
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
	public boolean should_importClass(XtendFile model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for(XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
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
	public boolean should_importClassesFrom(XtendFile model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for(XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
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
	public boolean should_importMembers(XtendFile model, String name) {
		if (model == null
				|| model.getImportSection() == null
				|| model.getImportSection().getImportDeclarations() == null) {
			return false;
		}
		for(XImportDeclaration d : model.getImportSection().getImportDeclarations()) {
			if (d != null && Objects.equals(name, d.getImportedName())) {
				return d.isStatic() && d.isWildcard() && !d.isExtension();
			}
		}
		return false;
	}

	/** Ensure that the given object is the SARL "event" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the event.
	 * @return the validation status
	 */
	public boolean should_beEvent(EObject o, String name) {
		if (!(o instanceof SarlEvent)) {
			return false;
		}
		SarlEvent e = (SarlEvent) o;
		return Objects.equals(name, e.getName());
	}

	/** Ensure that the given object is the SARL "capacity" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the capacity.
	 * @return the validation status
	 */
	public boolean should_beCapacity(EObject o, String name) {
		if (!(o instanceof SarlCapacity)) {
			return false;
		}
		SarlCapacity c = (SarlCapacity) o;
		return Objects.equals(name, c.getName());
	}

	/** Ensure that the given object is the SARL "skill" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the skill.
	 * @return the validation status
	 */
	public boolean should_beSkill(EObject o, String name) {
		if (!(o instanceof SarlSkill)) {
			return false;
		}
		SarlSkill s = (SarlSkill) o;
		return Objects.equals(name, s.getName());
	}

	/** Ensure that the given object is the SARL "behavior" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the behavior.
	 * @return the validation status
	 */
	public boolean should_beBehavior(EObject o, String name) {
		if (!(o instanceof SarlBehavior)) {
			return false;
		}
		SarlBehavior b = (SarlBehavior) o;
		return Objects.equals(name, b.getName());
	}

	/** Ensure that the given object is the SARL "agent" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beAgent(EObject o, String name) {
		if (!(o instanceof SarlAgent)) {
			return false;
		}
		SarlAgent a = (SarlAgent) o;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is extending the given type.
	 *
	 * @param o - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 * if none.
	 * @return o
	 */
	public boolean should_extend(EObject o, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (o instanceof SarlAgent) {
			it = safeSingleton(((SarlAgent) o).getExtends()).iterator();
		} else if (o instanceof SarlBehavior) {
			it = safeSingleton(((SarlBehavior) o).getExtends()).iterator();
		} else if (o instanceof SarlCapacity) {
			it = safeList(((SarlCapacity) o).getExtends()).iterator();
		} else if (o instanceof SarlEvent) {
			it = safeSingleton(((SarlEvent) o).getExtends()).iterator();
		} else if (o instanceof SarlSkill) {
			it = safeSingleton(((SarlSkill) o).getExtends()).iterator();
		} else if (o instanceof XtendClass) {
			it = safeSingleton(((XtendClass) o).getExtends()).iterator();
		} else if (o instanceof XtendInterface) {
			it = safeList(((XtendInterface) o).getExtends()).iterator();
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
	 * @param o - the object to test.
	 * @param superTypes - the names of the expected super-types, or <code>null</code>
	 * if none.
	 * @return o
	 */
	public boolean should_implement(EObject o, Object superTypes) {
		Iterator<? extends JvmTypeReference> it;
		if (o instanceof SarlSkill) {
			it = safeList(((SarlSkill) o).getImplements()).iterator();
		} else if (o instanceof XtendClass) {
			it = safeList(((XtendClass) o).getImplements()).iterator();
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
	 * @param o - the object to test.
	 * @param numberOfImplements - the number of implemented types.
	 * @return o
	 */
	public boolean should_haveNbImplements(EObject o, int numberOfImplements) {
		int n;
		if (o instanceof SarlSkill) {
			n = safeList(((SarlSkill) o).getImplements()).size();
		} else if (o instanceof XtendClass) {
			n = safeList(((XtendClass) o).getImplements()).size();
		} else {
			return false;
		}
		return n == numberOfImplements;
	}

	/** Ensure that the given object is the SARL "var" statement.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	public boolean should_beVariable(EObject o, String name) {
		if (!(o instanceof XtendField)) {
			return false;
		}
		XtendField attr = (XtendField) o;
		return Objects.equals(name, attr.getName())
				&& !attr.isFinal();
	}

	/** Ensure that the given object is the SARL "val" statement.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the attribute.
	 * @return the validation status.
	 */
	public boolean should_beValue(EObject o, String name) {
		if (!(o instanceof XtendField)) {
			return false;
		}
		XtendField attr = (XtendField) o;
		return Objects.equals(name, attr.getName())
				&& attr.isFinal();
	}

	/** Ensure that the given object is the SARL "var" or "val" statement
	 * with the given type.
	 *
	 * @param o - the object to test.
	 * @param type - the expected type name of the attribute, or <code>null</code>.
	 * @return the validation status.
	 */
	public boolean should_haveType(EObject o, String type) {
		if (o instanceof XtendField) {
			XtendField attr = (XtendField) o;
			if (type == null) {
				return attr.getType() == null;
			}
			return attr.getType() != null
					&& Objects.equals(type, attr.getType().getQualifiedName());
		} else if (o instanceof XtendParameter) {
			XtendParameter param = (XtendParameter) o;
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
	 * @param o - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	public boolean should_beAction(EObject o, String name) {
		if (!(o instanceof XtendFunction)) {
			return false;
		}
		XtendFunction act = (XtendFunction) o;
		return Objects.equals(name, act.getName()) && act.getExpression() != null;
	}

	/** Ensure that the given object is the SARL "def" statement (without body).
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the action.
	 * @return validation status
	 */
	public boolean should_beActionSignature(EObject o, String name) {
		if (!(o instanceof XtendFunction)) {
			return false;
		}
		XtendFunction sig = (XtendFunction) o;
		return Objects.equals(name, sig.getName()) && sig.getExpression() == null;
	}

	/** Ensure that the given object is the SARL "on" statement.
	 *
	 * @param o - the object to test.
	 * @param event - the name of expected event.
	 * @return validation status
	 */
	public boolean should_beBehaviorUnit(EObject o, String event) {
		if (!(o instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) o;
		return Objects.equals(event, bu.getName().getQualifiedName());
	}

	/** Ensure that the given object is the SARL "uses" statement.
	 *
	 * @param o - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	public boolean should_beCapacityUse(EObject o, Object capacities) {
		if (!(o instanceof SarlCapacityUses)) {
			return false;
		}
		SarlCapacityUses uses = (SarlCapacityUses) o;
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
	 * @param o - the object to test.
	 * @param capacities - the collection of the expected capacities.
	 * @return validation status
	 */
	public boolean should_beCapacityRequirement(EObject o, Object capacities) {
		if (!(o instanceof SarlRequiredCapacity)) {
			return false;
		}
		SarlRequiredCapacity reqs = (SarlRequiredCapacity) o;
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
	 * @param o - the object to test.
	 * @param guard - the expected guard.
	 * @return validation status
	 */
	public boolean should_beGuardedWith(EObject o, String guard) {
		if (!(o instanceof SarlBehaviorUnit)) {
			return false;
		}
		SarlBehaviorUnit bu = (SarlBehaviorUnit) o;
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
	 * @param o - the object to test.
	 * @param something - not used.
	 * @return validation status
	 */
	public boolean should_beConstructor(EObject o, Object something) {
		if (!(o instanceof XtendConstructor)) {
			return false;
		}
		return true;
	}

	/** Ensure that the given object is the SARL "def" statement
	 * that is variadic or not.
	 *
	 * @param o - the object to test.
	 * @param isVariadic - the expected variadic flag
	 * @return validation status
	 */
	public boolean should_beVariadic(EObject o, boolean isVariadic) {
		XtendExecutable f;
		if (o instanceof XtendExecutable) {
			f = (XtendExecutable) o;
		} else {
			return false;
		}
		boolean i;
		if (f.getParameters() == null || f.getParameters().isEmpty()) {
			i = false;
		} else {
			i = f.getParameters().get(f.getParameters().size() - 1).isVarArg();
		}
		return i == isVariadic;
	}

	/** Ensure that the given object is the SARL "def" statement (with body)
	 * that is returning the given type.
	 *
	 * @param o - the object to test.
	 * @param returnType - the name of the expected return type.
	 * @return the validation status
	 */
	public boolean should_reply(EObject o, String returnType) {
		JvmTypeReference rType;
		if (o instanceof XtendFunction) {
			rType = ((XtendFunction) o).getReturnType();
		} else {
			return false;
		}
		if (returnType == null) {
			return rType == null;
		}
		return rType != null
				&& Objects.equals(returnType, rType.getQualifiedName());
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param o - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	public boolean should_beParameter(EObject o, String name) {
		if (!(o instanceof XtendParameter)) {
			return false;
		}
		XtendParameter p = (XtendParameter) o;
		return Objects.equals(name, p.getName());
	}

	/** Ensure that the given feature has a formal parameter.
	 *
	 * @param o - the feature to test.
	 * @param name - the expected name of the formal parameter.
	 * @return the validation status
	 */
	public boolean should_haveDefaultValue(EObject o, Object name) {
		if (!(o instanceof SarlFormalParameter)) {
			return false;
		}
		SarlFormalParameter p = (SarlFormalParameter) o;
		if (name == null) {
			return p.getDefaultValue() == null;
		}
		return SpecificationTools.should_beLiteral(p.getDefaultValue(), name);
	}

	/** Ensure that the given feature is an attribute with an initial value.
	 *
	 * @param o - the feature to test.
	 * @param initialValue - the expected literal for the initial value.
	 * @return the validation status
	 */
	public boolean should_haveInitialValue(EObject o, Object initialValue) {
		if (!(o instanceof XtendField)) {
			return false;
		}
		XtendField p = (XtendField) o;
		if (initialValue == null) {
			return p.getInitialValue() == null;
		}
		XExpression expr = p.getInitialValue();
		if ((expr instanceof XFeatureCall) || (expr instanceof XMemberFeatureCall)
				|| (expr instanceof XConstructorCall)) {
			return should_call(expr, initialValue.toString());
		}
		if (expr instanceof XCastedExpression) {
			XCastedExpression e = (XCastedExpression) expr;
			if (Objects.equals(
					e.getType().getQualifiedName(),
					initialValue.toString())) {
				return true;
			}
			return SpecificationTools.should_beLiteral(e.getTarget(), initialValue);
		}
		return SpecificationTools.should_beLiteral(p.getInitialValue(), initialValue);
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
			XMemberFeatureCall c = (XMemberFeatureCall) actual;
			return Objects.equals(expected, c.getFeature().getQualifiedName());
		}
		if (actual instanceof XFeatureCall) {
			XFeatureCall c = (XFeatureCall) actual;
			return Objects.equals(expected, c.getFeature().getQualifiedName());
		}
		if (actual instanceof XConstructorCall) {
			XConstructorCall c = (XConstructorCall) actual;
			String consName = c.getConstructor().getQualifiedName() +
					"." + c.getConstructor().getSimpleName(); //$NON-NLS-1$
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
	 * to not care.
	 * @return the XExpression that is corresponding to the given expression.
	 * @throws Exception if the expression cannot be parsed.
	 */
	public XExpression expression(String expression, boolean resolve) throws Exception {
		String code = "def ____TeStInG_FuNcTiOn() : Object {\n" //$NON-NLS-1$
				+ expression
				+ "\n}"; //$NON-NLS-1$
		XtendFunction action = (XtendFunction) agentCode("AgentXXXXX", code, resolve).get(0); //$NON-NLS-1$
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
	 * to not care.
	 * @return the statements in the agent definition.
	 * @throws Exception if the code cannot be parsed.
	 */
	public List<? extends EObject> agentCode(String agentTypeName, String code, boolean resolve) throws Exception {
		String fullCode = "agent " + agentTypeName //$NON-NLS-1$
				+ " {\n" + code //$NON-NLS-1$
				+ "\n}\n"; //$NON-NLS-1$
		XtendFile script = parse(fullCode);
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
		IEvaluationResult r = this.interpreter.evaluate(expr);
		if (r == null) {
			throw new RuntimeException("cannot evaluate"); //$NON-NLS-1$
		}
		Throwable e = r.getException();
		if (e != null) {
			if (e instanceof Exception) {
				throw (Exception) e;
			} else if (e instanceof Error) {
				throw (Error) e;
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a number.
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
	 * @throws Exception if the expression cannot be parsed or is not a character.
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
	 * @throws Exception if the expression cannot be parsed or is not a boolean.
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
	 * @throws Exception if the expression cannot be parsed.
	 */
	public String toStr(String expression) throws Exception {
		Object n = to(expression, Object.class);
		if (n != null) {
			return n.toString();
		}
		return null;
	}

	/** Ensure that the given object is the SARL "class" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beClass(EObject o, String name) {
		if (!(o instanceof XtendClass)) {
			return false;
		}
		XtendClass a = (XtendClass) o;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "interface" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beInterface(EObject o, String name) {
		if (!(o instanceof XtendInterface)) {
			return false;
		}
		XtendInterface a = (XtendInterface) o;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beEnumeration(EObject o, String name) {
		if (!(o instanceof XtendEnum)) {
			return false;
		}
		XtendEnum a = (XtendEnum) o;
		return Objects.equals(name, a.getName());
	}

	/** Ensure that the given object is the SARL "enumeration" top element.
	 *
	 * @param o - the object to test.
	 * @param name - the expected name of the agent.
	 * @return validation status
	 */
	public boolean should_beAnnotation(EObject o, String name) {
		if (!(o instanceof XtendAnnotationType)) {
			return false;
		}
		XtendAnnotationType a = (XtendAnnotationType) o;
		return Objects.equals(name, a.getName());
	}

}
