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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.ui.labeling;

import javax.inject.Singleton;

import com.google.inject.Inject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.ide.labeling.XtendDescriptionLabelProvider;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.xbase.ui.labeling.XbaseImageAdornments;
import org.eclipse.xtext.xtype.XImportDeclaration;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;

/**
 * Provides labels for a IEObjectDescriptions and IResourceDescriptions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#labelProvider"
 */
@Singleton
public class SARLDescriptionLabelProvider extends XtendDescriptionLabelProvider {

	@Inject
	private IXtendJvmAssociations jvmModelAssociations;

	@Inject
	private SARLImages images;

	@Inject
	private XbaseImageAdornments adornments;

	/** Replies the image for a SARL script.
	 *
	 * @param script the SARL script.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlScript script) {
		return this.images.forFile();
	}

	/** Replies the image for an import declaration.
	 *
	 * @param declaration describes the import declaration.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(XImportDeclaration declaration) {
		return this.images.forImport();
	}

	/** Replies the image for an agent.
	 *
	 * @param agent describes the agent.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlAgent agent) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(agent);
		return this.images.forAgent(
				agent.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for a behavior.
	 *
	 * @param behavior describes the behavior.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlBehavior behavior) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(behavior);
		return this.images.forBehavior(
				behavior.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for a capacity.
	 *
	 * @param capacity describes the capacity.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlCapacity capacity) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(capacity);
		return this.images.forCapacity(
				capacity.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for a skill.
	 *
	 * @param skill describes the skill.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlSkill skill) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(skill);
		return this.images.forSkill(
				skill.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for an event.
	 *
	 * @param event describes the event.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlEvent event) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(event);
		return this.images.forEvent(
				event.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for an action.
	 *
	 * @param action describes the action.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlAction action) {
		final JvmOperation jvmElement = this.jvmModelAssociations.getDirectlyInferredOperation(action);
		return this.images.forOperation(
				action.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for a capacity use.
	 *
	 * @param uses describes the capacity use.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlCapacityUses uses) {
		return this.images.forCapacityUses();
	}

	/** Replies the image for a required capacity.
	 *
	 * @param capacity describes the required capacity.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlRequiredCapacity capacity) {
		return this.images.forCapacityRequirements();
	}

	/** Replies the image for a behavior unit.
	 *
	 * @param unit describes the behavior unit.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlBehaviorUnit unit) {
		return this.images.forBehaviorUnit();
	}

	/** Replies the image for an attribute.
	 *
	 * @param attribute describes the attribute.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlField attribute) {
		return this.images.forField(
				attribute.getVisibility(),
				this.adornments.get(this.jvmModelAssociations.getJvmField(attribute)));
	}

	/** Replies the image for a constructor.
	 *
	 * @param constructor describes the constructor.
	 * @return the image descriptor.
	 */
	public ImageDescriptor image(SarlConstructor constructor) {
		if (constructor.isStatic()) {
			return this.images.forStaticConstructor();
		}
		return this.images.forConstructor(
				constructor.getVisibility(),
				this.adornments.get(this.jvmModelAssociations.getInferredConstructor(constructor)));
	}

	@Override
	public Object image(IEObjectDescription element) {
		return doGetImage(element.getEObjectOrProxy());
	}

}
