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
package io.sarl.lang.genmodel;


import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.util.ModelUtil;

import java.util.Collection;

import javax.inject.Named;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.JvmDeclaredType;
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
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;
import org.eclipse.xtext.xtype.XtypeFactory;

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
	private IResourceFactory resourceFactory;

	@Inject
	private ActionSignatureProvider actionSignatureProvider;

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
		SarlScript script = SarlFactory.eINSTANCE.createSarlScript();
		if (!Strings.isNullOrEmpty(packageName)) {
			script.setName(packageName);
		}
		EList<EObject> content = resource.getContents();
		if (!content.isEmpty()) {
			content.clear();
		}
		content.add(script);
		return new GeneratedCode(script, resource.getResourceSet());
	}

	/** Create a SARL agent in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param agentName - the name of the agent.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Agent.
	 * @return the SARL agent.
	 */
	public Agent createAgent(GeneratedCode code, String agentName, String superClass)  {
		Agent agent = SarlFactory.eINSTANCE.createAgent();
		agent.setName(agentName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Agent.class.getName().equals(superClass)) {
			agent.getSuperTypes().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getElements().add(agent);
		return agent;
	}

	/** Create a SARL behavior in the script.
	 *
	 * @param code - the generated code in which the behavior must be created.
	 * @param behaviorName - the name of the behavior.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Behavior.
	 * @return the SARL behavior.
	 */
	public Behavior createBehavior(GeneratedCode code, String behaviorName, String superClass)  {
		Behavior behavior = SarlFactory.eINSTANCE.createBehavior();
		behavior.setName(behaviorName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Behavior.class.getName().equals(superClass)) {
			behavior.getSuperTypes().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getElements().add(behavior);
		return behavior;
	}

	/** Create a SARL capacity in the script.
	 *
	 * @param code - the generated code in which the capacity must be created.
	 * @param capacityName - the name of the capacity.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Capacity.
	 * @return the SARL capacity.
	 */
	public Capacity createCapacity(GeneratedCode code, String capacityName, String superClass)  {
		Capacity capacity = SarlFactory.eINSTANCE.createCapacity();
		capacity.setName(capacityName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Capacity.class.getName().equals(superClass)) {
			capacity.getSuperTypes().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getElements().add(capacity);
		return capacity;
	}

	/** Create a SARL event in the script.
	 *
	 * @param code - the generated code in which the event must be created.
	 * @param eventName - the name of the event.
	 * @param superClass - the name of the super class, or <code>null</code> if the default Event.
	 * @return the SARL event.
	 */
	public Event createEvent(GeneratedCode code, String eventName, String superClass)  {
		Event event = SarlFactory.eINSTANCE.createEvent();
		event.setName(eventName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Event.class.getName().equals(superClass)) {
			event.getSuperTypes().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		code.getSarlScript().getElements().add(event);
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
	public Skill createSkill(GeneratedCode code, String skillName, String superClass, Collection<String> superInterfaces)  {
		Skill skill = SarlFactory.eINSTANCE.createSkill();
		skill.setName(skillName);
		if (!Strings.isNullOrEmpty(superClass)
				&& !io.sarl.lang.core.Skill.class.getName().equals(superClass)) {
			skill.getSuperTypes().add(newTypeRef(code, superClass, code.getSarlScript()));
		}
		if (!superInterfaces.isEmpty()) {
			EList<JvmTypeReference> interfaces = skill.getImplementedTypes();
			for (String superInterface : superInterfaces) {
				if (!Strings.isNullOrEmpty(superInterface)
						&& !io.sarl.lang.core.Capacity.class.getName().equals(superInterface)) {
					interfaces.add(newTypeRef(code, superInterface, code.getSarlScript()));
				}
			}
		}
		code.getSarlScript().getElements().add(skill);
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
	public BehaviorUnit createBehaviorUnit(GeneratedCode code, FeatureContainer container,
			String eventName, XExpression guard, XBlockExpression block)  {
		BehaviorUnit unit = SarlFactory.eINSTANCE.createBehaviorUnit();
		unit.setName(newTypeRef(code, eventName, container));
		if (guard != null) {
			unit.setGuard(guard);
		}
		XExpression b = block;
		if (b == null) {
			b = XbaseFactory.eINSTANCE.createXBlockExpression();
		}
		unit.setBody(b);
		container.getFeatures().add(unit);
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
	public Attribute createVariable(GeneratedCode code, FeatureContainer container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		Attribute attribute = SarlFactory.eINSTANCE.createAttribute();
		attribute.setWriteable(true);
		attribute.setName(name);
		attribute.setType(newTypeRef(code, type, container));
		container.getFeatures().add(attribute);
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
	@SuppressWarnings("static-method")
	public Attribute createVariable(GeneratedCode code, FeatureContainer container, String name, XExpression initialValue)  {
		if (initialValue == null) {
			throw new IllegalArgumentException("the parameter 'initialValue' must not be null"); //$NON-NLS-1$
		}
		Attribute attribute = SarlFactory.eINSTANCE.createAttribute();
		attribute.setWriteable(true);
		attribute.setName(name);
		attribute.setInitialValue(initialValue);
		container.getFeatures().add(attribute);
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
	public Attribute createValue(GeneratedCode code, FeatureContainer container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		Attribute attribute = SarlFactory.eINSTANCE.createAttribute();
		attribute.setWriteable(false);
		attribute.setName(name);
		attribute.setType(newTypeRef(code, type, container));
		container.getFeatures().add(attribute);
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
	@SuppressWarnings("static-method")
	public Attribute createValue(GeneratedCode code, FeatureContainer container, String name, XExpression initialValue)  {
		if (initialValue == null) {
			throw new IllegalArgumentException("the parameter 'initialValue' must not be null"); //$NON-NLS-1$
		}
		Attribute attribute = SarlFactory.eINSTANCE.createAttribute();
		attribute.setWriteable(false);
		attribute.setName(name);
		attribute.setInitialValue(initialValue);
		container.getFeatures().add(attribute);
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
	public FormalParameter createFormalParameter(GeneratedCode code, ParameterizedFeature container,
			String name, String type, XExpression defaultValue)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		FormalParameter parameter = SarlFactory.eINSTANCE.createFormalParameter();
		parameter.setName(name);
		parameter.setParameterType(newTypeRef(code, type, container));
		if (defaultValue != null) {
			parameter.setDefaultValue(defaultValue);
		}
		container.getParams().add(parameter);
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
	public FormalParameter createFormalParameter(GeneratedCode code, ParameterizedFeature container,
			String name, String type, String defaultValue, ResourceSet resourceSet)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		return createFormalParameter(code, container, name, type,
				createXExpression(code, defaultValue, resourceSet));
	}

	/** Create an expression.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param expression - the texutal representation of the expression.
	 * @param resourceSet - the set of resources in which this function could
	 * create temporary resources for compiling the default value.
	 * @return the SARL formal parameter.
	 */
	public XExpression createXExpression(GeneratedCode code, String expression, ResourceSet resourceSet)  {
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
					Agent ag = (Agent) ((SarlScript) content).getElements().get(0);
					Attribute attr = (Attribute) ag.getFeatures().get(0);
					xExpression = attr.getInitialValue();
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
	public FormalParameter createVarArgs(GeneratedCode code, ParameterizedFeature container, String name, String type)  {
		if (Strings.isNullOrEmpty(type)) {
			throw new IllegalArgumentException("the parameter 'type' must contains a valid type"); //$NON-NLS-1$
		}
		FormalParameter parameter = SarlFactory.eINSTANCE.createFormalParameter();
		parameter.setName(name);
		parameter.setParameterType(newTypeRef(code, type, container));
		container.getParams().add(parameter);
		container.setVarargs(true);
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
	public Action createAction(GeneratedCode code, FeatureContainer container, String actionName,
			String returnType, XBlockExpression block)  {
		Action action = SarlFactory.eINSTANCE.createAction();
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
		action.setBody(b);
		if (!Strings.isNullOrEmpty(defaultValue)) {
			action.setType(newTypeRef(code, returnType, container));
		}
		container.getFeatures().add(action);
		return action;
	}

	/** Create a SARL action signature in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param actionName - the name of the action.
	 * @param returnType - the name of the return type, or <code>null</code> if the action's return type is void.
	 * @return the SARL action signature.
	 */
	public ActionSignature createActionSignature(GeneratedCode code, FeatureContainer container, String actionName,
			String returnType)  {
		ActionSignature actionSignature = SarlFactory.eINSTANCE.createActionSignature();
		actionSignature.setName(actionName);
		String defaultValue = ModelUtil.getDefaultValueForType(returnType);
		if (!Strings.isNullOrEmpty(defaultValue)) {
			actionSignature.setType(newTypeRef(code, returnType, container));
		}
		container.getFeatures().add(actionSignature);
		return actionSignature;
	}

	/** Create a SARL constructor in the script.
	 *
	 * @param code - the generated code in which the agent must be created.
	 * @param container - the container of the feature.
	 * @param block - the code for the action, or <code>null</code> for an empty block.
	 * @return the SARL action.
	 */
	@SuppressWarnings("static-method")
	public Constructor createConstructor(GeneratedCode code, FeatureContainer container, XBlockExpression block)  {
		Constructor constructor = SarlFactory.eINSTANCE.createConstructor();
		XBlockExpression b = block;
		if (b == null) {
			b = XbaseFactory.eINSTANCE.createXBlockExpression();
		}
		constructor.setBody(b);
		container.getFeatures().add(constructor);
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
			switch(type) {
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

	/** Describes a generated code.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public class GeneratedCode {

		private final ImportManager importManager = new ImportManager();
		private final SarlScript script;
		private final ResourceSet resourceSet;

		/**
		 * @param script - the root Ecore element.
		 * @param resourceSet - the resource set in which the script should be generated.
		 */
		public GeneratedCode(SarlScript script, ResourceSet resourceSet) {
			this.script = script;
			this.resourceSet = resourceSet;
		}

		/** Replies the resource set in which the generated code should be generated.
		 *
		 * @return the resource set of the SARL script.
		 */
		public ResourceSet getResourceSet() {
			return this.resourceSet;
		}

		/** Replies the SARL code generator that had built this code.
		 *
		 * @return the SARL code generator.
		 */
		public SARLCodeGenerator getCodeGenerator() {
			return SARLCodeGenerator.this;
		}

		/** Replies the SARL script.
		 *
		 * @return the SARL script.
		 */
		public SarlScript getSarlScript() {
			return this.script;
		}

		/** Replies the import manager.
		 *
		 * @return the import manager.
		 */
		public ImportManager getImportManager() {
			return this.importManager;
		}

		/** Finialize the script.
		 *
		 * The finalization includes: <ul>
		 * <li>The import section is created.</li>
		 * </ul>
		 */
		public void finalizeScript() {
			XImportSection importSection = this.script.getImportSection();
			for (String importName : this.importManager.getImports()) {
				XImportDeclaration declaration = XtypeFactory.eINSTANCE.createXImportDeclaration();
				JvmType type = getTypeReferences().findDeclaredType(importName, this.script);
				if (type instanceof JvmDeclaredType) {
					declaration.setImportedType((JvmDeclaredType) type);
					if (importSection == null) {
						importSection = XtypeFactory.eINSTANCE.createXImportSection();
						this.script.setImportSection(importSection);
					}
					importSection.getImportDeclarations().add(declaration);
				}
			}
		}

	}

	/** Postfix documentation for an Ecore element.
	 *
	 * The prefix document is supported by {@link DocumentationAdapter}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see DocumentationAdapter
	 */
	public static class PostDocumentationAdapter extends AdapterImpl {

		private String documentation;

		/** Replies the documentation.
		 *
		 * @return the documentation.
		 */
		public String getDocumentation() {
			return this.documentation;
		}

		/** Change the documentation.
		 *
		 * @param documentation - the comment.
		 */
		public void setDocumentation(String documentation) {
			this.documentation = documentation;
		}

		@Override
		public boolean isAdapterForType(Object type) {
			return type == PostDocumentationAdapter.class;
		}

	}

	/** Documentation at the beginning of an Ecore block.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see DocumentationAdapter
	 */
	public static class BlockInnerDocumentationAdapter extends AdapterImpl {

		private String documentation;

		/** Replies the documentation.
		 *
		 * @return the documentation.
		 */
		public String getDocumentation() {
			return this.documentation;
		}

		/** Change the documentation.
		 *
		 * @param documentation - the comment.
		 */
		public void setDocumentation(String documentation) {
			this.documentation = documentation;
		}

		@Override
		public boolean isAdapterForType(Object type) {
			return type == BlockInnerDocumentationAdapter.class;
		}

	}

}
