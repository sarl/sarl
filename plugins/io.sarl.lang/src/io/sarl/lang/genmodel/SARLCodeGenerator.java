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
package io.sarl.lang.genmodel;


import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.util.ModelUtil;

import java.util.Collection;
import java.util.List;

import javax.inject.Named;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendFactory;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericArrayTypeReference;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/** Utilities for creating SARL elements.
 *
 * This class may use the XtendTypeCreatorUtil.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLCodeGenerator {

	@Inject
	private TypesFactory typeFactory;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private JvmTypesBuilder typesBuilder;

	@Inject
	private IResourceFactory resourceFactory;

	@Inject
	private ActionSignatureProvider actionSignatureProvider;

	@Inject
	private SARLGrammarAccess grammarAccess;

	private final String sarlFileExtension;

	/**
	 * @param fileExtension - the file extension for SARL scripts.
	 */
	@Inject
	public SARLCodeGenerator(@Named(Constants.FILE_EXTENSIONS) String fileExtension) {
		this.sarlFileExtension = fileExtension;
	}

	/** Replies the file extension of the SARL scripts.
	 *
	 * @return the SARL file extension.
	 */
	public String getSARLFileExtension() {
		return this.sarlFileExtension;
	}

	/** Replies the factory of type references.
	 *
	 * @return the factory of type references.
	 */
	public TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Replies the factory of types.
	 *
	 * @return the factory of types.
	 */
	public TypesFactory getTypesFactory() {
		return this.typeFactory;
	}

	/** Replies the type builder.
	 *
	 * @return the type builder.
	 */
	public JvmTypesBuilder getTypesBuilder() {
		return this.typesBuilder;
	}

	/** Replies the factory of resources.
	 *
	 * @return the factory of resources.
	 */
	public IResourceFactory getResourceFactory() {
		return this.resourceFactory;
	}

	/** Replies the SARL action signature provider.
	 *
	 * @return the signature provider.
	 */
	public ActionSignatureProvider getActionSignatureProvider() {
		return this.actionSignatureProvider;
	}

	/** Attach the given comment to the Ecore element.
	 *
	 * The comment will be displayed just before the Ecore element.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param object - the Ecore element to which the comment must be associated.
	 * @param comment - the comment.
	 */
	@SuppressWarnings("static-method")
	public void attachComment(GeneratedCode code, EObject object, String comment) {
		if (object != null && !Strings.isNullOrEmpty(comment)) {
			DocumentationAdapter documentationAdapter = new DocumentationAdapter();
			documentationAdapter.setDocumentation(comment);
			object.eAdapters().add(documentationAdapter);
		}
	}

	/** Attach the given comment to the Ecore element.
	 *
	 * The comment will be displayed just after the Ecore element.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param object - the Ecore element to which the comment must be associated.
	 * @param comment - the comment.
	 */
	@SuppressWarnings("static-method")
	public void attachPostComment(GeneratedCode code, EObject object, String comment) {
		if (object != null && !Strings.isNullOrEmpty(comment)) {
			PostDocumentationAdapter documentationAdapter = new PostDocumentationAdapter();
			documentationAdapter.setDocumentation(comment);
			object.eAdapters().add(documentationAdapter);
		}
	}

	/** Attach the given comment to the Ecore block.
	 *
	 * The comment will be displayed inside the block, as the first element inside.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param object - the Ecore element to which the comment must be associated.
	 * @param comment - the comment.
	 */
	@SuppressWarnings("static-method")
	public void attachInnerComment(GeneratedCode code, XBlockExpression object, String comment) {
		if (object != null && !Strings.isNullOrEmpty(comment)) {
			BlockInnerDocumentationAdapter documentationAdapter = new BlockInnerDocumentationAdapter();
			documentationAdapter.setDocumentation(comment);
			object.eAdapters().add(documentationAdapter);
		}
	}

	/** Create an empty SARL script, and put it in the given resource.
	 *
	 * If the given resource has already a content, it is removed and replaced
	 * by the new SARL script.
	 *
	 * @param resource - the Ecore resource for the script.
	 * @param packageName - the name of the package in the script.
	 * @return the SARL script.
	 */
	public GeneratedCode createScript(Resource resource, String packageName)  {
		XtendFile script = XtendFactory.eINSTANCE.createXtendFile();
		if (!Strings.isNullOrEmpty(packageName)) {
			script.setPackage(packageName);
		}
		EList<EObject> content = resource.getContents();
		if (!content.isEmpty()) {
			content.clear();
		}
		content.add(script);
		return new GeneratedCode(this, script, resource.getResourceSet(), getTypeReferences());
	}

	/** Create a SARL agent in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param agentName - the name of the agent.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Agent.
	 * @return the SARL agent.
	 */
	public SarlAgent createAgent(GeneratedCode code, String agentName, String superClass)  {
		SarlAgent agent = SarlFactory.eINSTANCE.createSarlAgent();
		agent.setName(agentName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Agent.class.getName().equals(superClass)) {
			agent.getExtends().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getXtendTypes().add(agent);
		return agent;
	}

	/** Create a SARL behavior in the script.
	 *
	 * @param code - the generated code in which the behavior must be created.
	 * @param behaviorName - the name of the behavior.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Behavior.
	 * @return the SARL behavior.
	 */
	public SarlBehavior createBehavior(GeneratedCode code, String behaviorName, String superClass)  {
		SarlBehavior behavior = SarlFactory.eINSTANCE.createSarlBehavior();
		behavior.setName(behaviorName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Behavior.class.getName().equals(superClass)) {
			behavior.getExtends().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getXtendTypes().add(behavior);
		return behavior;
	}

	/** Create a SARL capacity in the script.
	 *
	 * @param code - the generated code in which the capacity must be created.
	 * @param capacityName - the name of the capacity.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Capacity.
	 * @return the SARL capacity.
	 */
	public SarlCapacity createCapacity(GeneratedCode code, String capacityName, String superClass)  {
		SarlCapacity capacity = SarlFactory.eINSTANCE.createSarlCapacity();
		capacity.setName(capacityName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Capacity.class.getName().equals(superClass)) {
			capacity.getExtends().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getXtendTypes().add(capacity);
		return capacity;
	}

	/** Create a SARL event in the script.
	 *
	 * @param code - the generated code in which the event must be created.
	 * @param eventName - the name of the event.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Event.
	 * @return the SARL event.
	 */
	public SarlEvent createEvent(GeneratedCode code, String eventName, String superClass)  {
		SarlEvent event = SarlFactory.eINSTANCE.createSarlEvent();
		event.setName(eventName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Event.class.getName().equals(superClass)) {
			event.getExtends().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getXtendTypes().add(event);
		return event;
	}

	/** Create a SARL skill in the script.
	 *
	 * @param code - the generated code in which the skill must be created.
	 * @param skillName - the name of the skill.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Skill.
	 * @param superInterfaces - the names of the super interfaces of the implemented capacities.
	 * @return the SARL skill.
	 */
	public SarlSkill createSkill(GeneratedCode code, String skillName, String superClass, Collection<String> superInterfaces)  {
		SarlSkill skill = SarlFactory.eINSTANCE.createSarlSkill();
		skill.setName(skillName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Skill.class.getName().equals(superClass)) {
			skill.getExtends().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		if (!superInterfaces.isEmpty()) {
			EList<JvmParameterizedTypeReference> interfaces = skill.getImplements();
			for (String superInterface : superInterfaces) {
				if (!Strings.isNullOrEmpty(superInterface)
						&& !io.sarl.lang.core.Capacity.class.getName().equals(superInterface)) {
					interfaces.add(newTypeRef(code, superInterface, code.getSarlScript()));
				}
			}
		}
		code.getSarlScript().getXtendTypes().add(skill);
		return skill;
	}

	/** Create a SARL behavior unit in given container.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param eventName - the name of the event.
	 * @param guard - the guard, or <code>null</code> if none.
	 * @param block - the code for the event handler, or <code>null</code> for an empty block.
	 * @return the SARL behavior unit.
	 */
	public SarlBehaviorUnit createBehaviorUnit(GeneratedCode code, XtendTypeDeclaration container,
			String eventName, XExpression guard, XBlockExpression block)  {
		SarlBehaviorUnit unit = SarlFactory.eINSTANCE.createSarlBehaviorUnit();
		unit.setName(newTypeRef(code, eventName, container));
		if (guard != null) {
			unit.setGuard(guard);
		}
		XExpression b = block;
		if (b == null) {
			b = XbaseFactory.eINSTANCE.createXBlockExpression();
		}
		unit.setExpression(b);
		container.getMembers().add(unit);
		return unit;
	}

	/** Create a SARL read-write attribute in the container.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param name - the name of the attribute.
	 * @param type - the name of the return type, or <code>null</code> if the action's return type is void.
	 * @return the SARL attribute.
	 */
	public XtendField createVariable(GeneratedCode code, XtendTypeDeclaration container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		XtendField attribute = XtendFactory.eINSTANCE.createXtendField();
		attribute.getModifiers().add(this.grammarAccess.getFieldModifierAccess().getVarKeyword_1().getValue());
		attribute.setName(name);
		attribute.setType(newTypeRef(code, type, container));
		container.getMembers().add(attribute);
		return attribute;
	}

	/** Create a SARL read-write attribute in the container.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param name - the name of the attribute.
	 * @param initialValue - the initial value for the attribute.
	 * @return the SARL attribute.
	 */
	public XtendField createVariable(GeneratedCode code, XtendTypeDeclaration container, String name, XExpression initialValue)  {
		if (initialValue == null) {
			throw new IllegalArgumentException("the parameter 'initialValue' must not be null"); //$NON-NLS-1$
		}
		XtendField attribute = XtendFactory.eINSTANCE.createXtendField();
		attribute.getModifiers().add(this.grammarAccess.getFieldModifierAccess().getVarKeyword_1().getValue());
		attribute.setName(name);
		attribute.setInitialValue(initialValue);
		container.getMembers().add(attribute);
		return attribute;
	}

	/** Create a SARL read-only attribute in the container.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param name - the name of the attribute.
	 * @param type - the name of the return type, or <code>null</code> if the action's return type is void.
	 * @return the SARL attribute.
	 */
	public XtendField createValue(GeneratedCode code, XtendTypeDeclaration container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		XtendField attribute = XtendFactory.eINSTANCE.createXtendField();
		attribute.getModifiers().add(this.grammarAccess.getFieldModifierAccess().getValKeyword_0().getValue());
		attribute.setName(name);
		attribute.setType(newTypeRef(code, type, container));
		container.getMembers().add(attribute);
		return attribute;
	}

	/** Create a SARL read-only attribute in the container.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param name - the name of the attribute.
	 * @param initialValue - the initial value for the attribute.
	 * @return the SARL attribute.
	 */
	public XtendField createValue(GeneratedCode code, XtendTypeDeclaration container, String name, XExpression initialValue)  {
		if (initialValue == null) {
			throw new IllegalArgumentException("the parameter 'initialValue' must not be null"); //$NON-NLS-1$
		}
		XtendField attribute = XtendFactory.eINSTANCE.createXtendField();
		attribute.getModifiers().add(this.grammarAccess.getFieldModifierAccess().getValKeyword_0().getValue());
		attribute.setName(name);
		attribute.setInitialValue(initialValue);
		container.getMembers().add(attribute);
		return attribute;
	}

	/** Create a formal parameter.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the formal parameter.
	 * @param name - the name of the formal parameter.
	 * @param type - the name of the parameter type.
	 * @param defaultValue - the default value for the parameter.
	 * @return the SARL formal parameter.
	 */
	public SarlFormalParameter createFormalParameter(GeneratedCode code, XtendExecutable container,
			String name, String type, XExpression defaultValue)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		SarlFormalParameter parameter = SarlFactory.eINSTANCE.createSarlFormalParameter();
		parameter.setName(name);
		parameter.setParameterType(newTypeRef(code, type, container));
		if (defaultValue != null) {
			parameter.setDefaultValue(defaultValue);
		}
		container.getParameters().add(parameter);
		return parameter;
	}

	/** Compute a unused URI for a synthetic resource.
	 *
	 * @param resourceSet - the resource set in which the resource should be located.
	 * @return the uri.
	 */
	private URI computeUnusedUri(ResourceSet resourceSet) {
		String name = "__synthetic"; //$NON-NLS-1$
		for (int i = 0; i < Integer.MAX_VALUE; ++i) {
			URI syntheticUri = URI.createURI(name + i + "." + getSARLFileExtension()); //$NON-NLS-1$
			if (resourceSet.getResource(syntheticUri, false) == null) {
				return syntheticUri;
			}
		}
		throw new IllegalStateException();
	}

	/** Create a formal parameter.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the formal parameter.
	 * @param name - the name of the formal parameter.
	 * @param type - the name of the parameter type.
	 * @param defaultValue - the default value for the parameter.
	 * @param resourceSet - the set of resources in which this function could
	 * create temporary resources for compiling the default value.
	 * @return the SARL formal parameter.
	 */
	public SarlFormalParameter createFormalParameter(GeneratedCode code, XtendExecutable container,
			String name, String type, String defaultValue, ResourceSet resourceSet)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		return createFormalParameter(code, container, name, type,
				createXExpression(defaultValue, resourceSet, code.getImportManager()));
	}

	/** Create an expression.
	 *
	 * @param expression - the texutal representation of the expression.
	 * @param resourceSet - the set of resources in which this function could
	 * create temporary resources for compiling the default value.
	 * @param importManager - the manager of the imports that is used for importing the types.
	 * This parameter could be <code>null</code>.
	 * @return the SARL formal parameter.
	 */
	public XExpression createXExpression(String expression, ResourceSet resourceSet,
			ImportManager importManager)  {
		XExpression xExpression = null;
		if (!Strings.isNullOrEmpty(expression)) {
			URI uri = computeUnusedUri(resourceSet);
			Resource resource = getResourceFactory().createResource(uri);
			resourceSet.getResources().add(resource);
			StringBuilder realSarlCode = new StringBuilder();
			realSarlCode.append("agent STUBAGENT { val attr = "); //$NON-NLS-1$
			realSarlCode.append(expression);
			realSarlCode.append("; }"); //$NON-NLS-1$
			try (StringInputStream is = new StringInputStream(realSarlCode.toString())) {
				resource.load(is, null);
				EObject content = resource.getContents().isEmpty() ? null : resource.getContents().get(0);
				if (content != null) {
					XtendFile script = (XtendFile) content;
					SarlAgent ag = (SarlAgent) script.getXtendTypes().get(0);
					XtendField attr = (XtendField) ag.getMembers().get(0);
					xExpression = attr.getInitialValue();
					XImportSection importSection = script.getImportSection();
					if (importManager != null && importSection != null) {
						for (XImportDeclaration importDeclaration : importSection.getImportDeclarations()) {
							importManager.addImportFor(importDeclaration.getImportedType());
						}
					}
				}
			} catch (Throwable _) {
				//
			} finally {
				resourceSet.getResources().remove(resource);
			}
		}
		return xExpression;
	}

	/** Create a formal parameter.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the formal parameter.
	 * @param name - the name of the formal parameter.
	 * @param type - the name of the parameter type.
	 * @return the SARL formal parameter.
	 */
	public SarlFormalParameter createVarArgs(GeneratedCode code, XtendExecutable container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		SarlFormalParameter parameter = SarlFactory.eINSTANCE.createSarlFormalParameter();
		parameter.setName(name);
		parameter.setParameterType(newTypeRef(code, type, container));
		parameter.setVarArg(true);
		container.getParameters().add(parameter);
		return parameter;
	}

	/** Create a SARL action in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param actionName - the name of the action.
	 * @param returnType - the name of the return type, or <code>null</code> if the action's return type is void.
	 * @param block - the code for the action, or <code>null</code> for an empty block.
	 * @return the SARL action.
	 */
	public SarlAction createAction(GeneratedCode code, XtendTypeDeclaration container, String actionName,
			String returnType, XBlockExpression block)  {
		SarlAction action = SarlFactory.eINSTANCE.createSarlAction();
		action.setName(actionName);
		XBlockExpression b = block;
		String defaultValue = ModelUtil.getDefaultValueForType(returnType);
		if (b == null) {
			b = XbaseFactory.eINSTANCE.createXBlockExpression();
			if (!Strings.isNullOrEmpty(defaultValue)) {
				XExpression returnExpression = getDefaultXExpressionForType(code, container, returnType);
				attachComment(code, returnExpression,
						"TODO " //$NON-NLS-1$
						+ Messages.SARLCodeGenerator_0);
				b.getExpressions().add(returnExpression);
			} else {
				attachInnerComment(code, b,
						"TODO " //$NON-NLS-1$
						+ Messages.SARLCodeGenerator_0);
			}
		}
		action.setExpression(b);
		if (!Strings.isNullOrEmpty(defaultValue)) {
			action.setReturnType(newTypeRef(code, returnType, container));
		}
		container.getMembers().add(action);
		return action;
	}

	/** Create a SARL constructor in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param block - the code for the action, or <code>null</code> for an empty block.
	 * @return the SARL action.
	 */
	@SuppressWarnings("static-method")
	public XtendConstructor createConstructor(GeneratedCode code, XtendTypeDeclaration container, XBlockExpression block)  {
		XtendConstructor constructor = XtendFactory.eINSTANCE.createXtendConstructor();
		XBlockExpression b = block;
		if (b == null) {
			b = XbaseFactory.eINSTANCE.createXBlockExpression();
		}
		constructor.setExpression(b);
		container.getMembers().add(constructor);
		return constructor;
	}

	/** Replies the default value for the given type.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param context - the context where the expression should be located.
	 * @param type - the type for which the default value should be determined.
	 * @return the default value.
	 */
	public XExpression getDefaultXExpressionForType(GeneratedCode code, EObject context, String type) {
		//TODO: Check if a similar function exists in the Xbase library.
		XExpression expr = null;
		if (type != null && !"void".equals(type) && !Void.class.getName().equals(type)) { //$NON-NLS-1$
			switch (type) {
			case "boolean":  //$NON-NLS-1$
			case "java.lang.Boolean":  //$NON-NLS-1$
				XBooleanLiteral booleanLiteral = XbaseFactory.eINSTANCE.createXBooleanLiteral();
				booleanLiteral.setIsTrue(false);
				expr = booleanLiteral;
				break;
			case "float":  //$NON-NLS-1$
			case "java.lang.Float":  //$NON-NLS-1$
				XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
				numberLiteral.setValue("0.0f"); //$NON-NLS-1$
				expr = numberLiteral;
				break;
			case "double":  //$NON-NLS-1$
			case "java.lang.Double":  //$NON-NLS-1$
			case "java.lang.BigDecimal":  //$NON-NLS-1$
				numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
				numberLiteral.setValue("0.0"); //$NON-NLS-1$
				expr = numberLiteral;
				break;
			case "int":  //$NON-NLS-1$
			case "long":  //$NON-NLS-1$
			case "java.lang.Integer":  //$NON-NLS-1$
			case "java.lang.Long":  //$NON-NLS-1$
			case "java.lang.BigInteger":  //$NON-NLS-1$
				numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
				numberLiteral.setValue("0"); //$NON-NLS-1$
				expr = numberLiteral;
				break;
			case "byte": //$NON-NLS-1$
			case "short":  //$NON-NLS-1$
			case "char":  //$NON-NLS-1$
			case "java.lang.Byte":  //$NON-NLS-1$
			case "java.lang.Short":  //$NON-NLS-1$
			case "java.lang.Character":  //$NON-NLS-1$
				numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
				numberLiteral.setValue("0"); //$NON-NLS-1$
				XCastedExpression castExpression = XbaseFactory.eINSTANCE.createXCastedExpression();
				castExpression.setTarget(numberLiteral);
				castExpression.setType(newTypeRef(code, type, context));
				expr = numberLiteral;
				break;
			default:
				expr = XbaseFactory.eINSTANCE.createXNullLiteral();
				break;
			}
		}
		return expr;
	}

	/** Create a reference to the given type.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param typeName - the name of the type.
	 * @param context - the SARL context.
	 * @return the type reference.
	 */
	public JvmParameterizedTypeReference newTypeRef(GeneratedCode code, String typeName, EObject context) {
		TypeReferences typeRefs = getTypeReferences();
		JvmType type = typeRefs.findDeclaredType(typeName, context);
		if (type == null) {
			type = typeRefs.findDeclaredType("java.lang." + typeName, context); //$NON-NLS-1$
			if (type == null) {
				type = typeRefs.findDeclaredType("java.lang.Object", context); //$NON-NLS-1$
			}
		} else {
			code.getImportManager().addImportFor(type);
		}
		JvmParameterizedTypeReference reference = getTypesFactory().createJvmParameterizedTypeReference();
		reference.setType(type);
		return reference;
	}

	private JvmTypeReference cloneType(JvmTypeReference type) {
		// CAUTION: The following line is needed otherwise the clone of the type will failed.
		type.getIdentifier();
		return getTypesBuilder().cloneWithProxies(type);
	}

	/** Replies the SARL Ecore equivalent for the givne JVM Ecore element.
	 *
	 * @param operation - the JVM Ecore element.
	 * @param importManager - the manager of the imports that is used for importing the types.
	 * This parameter could be <code>null</code>.
	 * @return the SARL Ecore element.
	 */
	public SarlAction createAction(JvmOperation operation, ImportManager importManager) {
		SarlAction action = SarlFactory.eINSTANCE.createSarlAction();
		// Name
		action.setName(operation.getSimpleName());
		// Return types
		JvmTypeReference typeReference = cloneType(operation.getReturnType());
		action.setReturnType(typeReference);
		updateImports(typeReference, importManager);
		// Parameters
		createExecutableFeatureParameters(operation, action.getParameters(), importManager);
		// Throws
		createExecutableFeatureExceptions(operation, action.getExceptions(), importManager);
		// Fired events
		createExecutableFeatureFireEvents(operation, action.getFiredEvents(), importManager);
		return action;
	}

	private static void updateImports(JvmTypeReference type, ImportManager manager) {
		if (manager != null && type != null) {
			JvmType t = type.getType();
			if (t != null) {
				manager.addImportFor(t);
			}
		}
	}

	/** Replies the SARL Ecore equivalent for the givne JVM Ecore element.
	 *
	 * @param constructor - the JVM Ecore element.
	 * @param importManager - the manager of the imports that is used for importing the types.
	 * This parameter could be <code>null</code>.
	 * @return the SARL Ecore element.
	 */
	public XtendConstructor createConstructor(JvmConstructor constructor, ImportManager importManager) {
		XtendConstructor cons = XtendFactory.eINSTANCE.createXtendConstructor();
		// Parameters
		createExecutableFeatureParameters(constructor, cons.getParameters(), importManager);
		// Throws
		createExecutableFeatureExceptions(constructor, cons.getExceptions(), importManager);
		return cons;
	}

	private void createExecutableFeatureParameters(JvmExecutable operation, List<XtendParameter> parameters,
			ImportManager importManager) {
		List<JvmFormalParameter> jvmParameters = operation.getParameters();
		for (int i = 0; i < jvmParameters.size(); ++i) {
			JvmFormalParameter jvmParameter = jvmParameters.get(i);
			SarlFormalParameter parameter = SarlFactory.eINSTANCE.createSarlFormalParameter();
			parameter.setName(jvmParameter.getSimpleName());
			JvmTypeReference typeReference = jvmParameter.getParameterType();
			if (i == jvmParameters.size() - 1 && operation.isVarArgs()) {
				typeReference = ((JvmGenericArrayTypeReference) typeReference).getComponentType();
				parameter.setVarArg(true);
			}
			typeReference = cloneType(typeReference);
			parameter.setParameterType(typeReference);
			updateImports(typeReference, importManager);
			// Variadic parameter
			// Default values
			String defaultValue = findDefaultValue(
					operation.getDeclaringType(),
					ModelUtil.annotationString(jvmParameter, DefaultValue.class));
			if (!Strings.isNullOrEmpty(defaultValue)) {
				XExpression defaultValueExpr = createXExpression(
						defaultValue,
						operation.eResource().getResourceSet(),
						importManager);
				parameter.setDefaultValue(defaultValueExpr);
			}
			parameters.add(parameter);
		}
	}

	private void createExecutableFeatureFireEvents(JvmExecutable operation, List<JvmTypeReference> events,
			ImportManager importManager) {
		List<JvmTypeReference> firedEvents = ModelUtil.annotationClasses(operation, FiredEvent.class);
		if (!firedEvents.isEmpty()) {
			for (JvmTypeReference type : firedEvents) {
				JvmTypeReference clone = cloneType(type);
				if (clone instanceof JvmParameterizedTypeReference) {
					JvmParameterizedTypeReference pRef = (JvmParameterizedTypeReference) clone;
					updateImports(pRef, importManager);
					events.add(pRef);
				}
			}
		}
	}

	private void createExecutableFeatureExceptions(JvmExecutable operation, List<JvmTypeReference> exceptions,
			ImportManager importManager) {
		List<JvmTypeReference> jvmExceptions = operation.getExceptions();
		if (!jvmExceptions.isEmpty()) {
			for (JvmTypeReference type : jvmExceptions) {
				JvmTypeReference clone = cloneType(type);
				updateImports(clone, importManager);
				exceptions.add(clone);
			}
		}
	}

	private static String findDefaultValue(JvmDeclaredType container, String name) {
		if (!Strings.isNullOrEmpty(name)) {
			String dfName = ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE + name;
			for (JvmField field : container.getDeclaredFields()) {
				if (field.getSimpleName().equals(dfName)) {
					return ModelUtil.annotationString(field, Generated.class);
				}
			}
		}
		return null;
	}

}
