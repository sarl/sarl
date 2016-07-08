/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.lang.ui.contentassist;

import com.google.inject.Inject;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.util.RawSuperTypes;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.ui.IImageHelper;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalFactory;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.ui.images.SARLImages;

/** Provider of types to be imported.
 *
 * <p>This proposal does nothing special than the Xbase provider. But is is here allowing to filter types in SARL code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLImportingTypesProposalProvider extends ImportingTypesProposalProvider {

	private static final int ACC_AGENT = 0x1000000;

	private static final int ACC_BEHAVIOR = 0x2000000;

	private static final int ACC_CAPACITY = 0x4000000;

	private static final int ACC_SKILL = 0x8000000;

	private static final int ACC_EVENT = 0x10000000;

	private static final int ACC_ALL = ACC_AGENT | ACC_BEHAVIOR | ACC_CAPACITY | ACC_SKILL | ACC_EVENT;

	@Inject
	private SARLImages images;

	@Inject
	private IImageHelper imageHelper;

	@Inject
	private RawSuperTypes superTypeCollector;

	private static int removeFlags(int modifiers) {
		return (modifiers & ACC_ALL) ^ modifiers;
	}

	private static int addFlag(int modifiers, int flag) {
		assert (modifiers & flag) == 0;
		return modifiers | flag;
	}

	private static JvmVisibility toJVMVisibility(int modifiers) {
		if (Flags.isPublic(modifiers)) {
			return JvmVisibility.PUBLIC;
		}
		if (Flags.isPrivate(modifiers)) {
			return JvmVisibility.PRIVATE;
		}
		if (Flags.isProtected(modifiers)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.DEFAULT;
	}

	private static int toAdornments(int flags, boolean allowAbstract) {
		int adornments = 0;
		if (Flags.isAbstract(flags) && allowAbstract) {
			adornments |= JavaElementImageDescriptor.ABSTRACT;
		}
		if (Flags.isFinal(flags)) {
			adornments |= JavaElementImageDescriptor.FINAL;
		}
		if (Flags.isStatic(flags)) {
			adornments |= JavaElementImageDescriptor.STATIC;
		}
		if (Flags.isDeprecated(flags)) {
			adornments |= JavaElementImageDescriptor.DEPRECATED;
		}
		return adornments;
	}

	private boolean isAssignableTo(JvmGenericType source, Class<?> target, IJvmTypeProvider jvmTypeProvider) {
		final String name = target.getName();
		if (name.equals(source.getIdentifier())) {
			return true;
		}
		final JvmType targetType = jvmTypeProvider.findTypeByName(name);
		return this.superTypeCollector.collect(source).contains(targetType);
	}

	@Override
	protected void createTypeProposal(String typeName, int modifiers, boolean isInnerType,
			ICompletionProposalFactory proposalFactory, ContentAssistContext context, ICompletionProposalAcceptor acceptor,
			IJvmTypeProvider jvmTypeProvider, IValueConverter<String> valueConverter) {
		int updatedModifiers = modifiers;
		final JvmType type = jvmTypeProvider.findTypeByName(typeName);
		if (type.eClass() == TypesPackage.Literals.JVM_GENERIC_TYPE) {
			final JvmGenericType gtype = (JvmGenericType) type;
			if (gtype.isInterface()) {
				if (isAssignableTo(gtype, Capacity.class, jvmTypeProvider)) {
					updatedModifiers = addFlag(modifiers, ACC_CAPACITY);
				}
			} else if (isAssignableTo(gtype, Agent.class, jvmTypeProvider)) {
				updatedModifiers = addFlag(modifiers, ACC_AGENT);
			} else if (isAssignableTo(gtype, Behavior.class, jvmTypeProvider)) {
				updatedModifiers = addFlag(modifiers, ACC_BEHAVIOR);
			} else if (isAssignableTo(gtype, Skill.class, jvmTypeProvider)) {
				updatedModifiers = addFlag(modifiers, ACC_SKILL);
			} else if (isAssignableTo(gtype, Event.class, jvmTypeProvider)) {
				updatedModifiers = addFlag(modifiers, ACC_EVENT);
			}
		}
		super.createTypeProposal(typeName, updatedModifiers, isInnerType, proposalFactory, context,
				acceptor, jvmTypeProvider, valueConverter);
	}

	@Override
	protected Image computeImage(String typeName, boolean isInnerType, int modifiers) {
		if ((modifiers & ACC_AGENT) != 0) {
			return this.imageHelper.getImage(this.images.forAgent(toJVMVisibility(modifiers),
					toAdornments(removeFlags(modifiers), true)));
		}
		if ((modifiers & ACC_BEHAVIOR) != 0) {
			return this.imageHelper.getImage(this.images.forBehavior(toJVMVisibility(modifiers),
					toAdornments(removeFlags(modifiers), true)));
		}
		if ((modifiers & ACC_CAPACITY) != 0) {
			return this.imageHelper.getImage(this.images.forCapacity(toJVMVisibility(modifiers),
					toAdornments(removeFlags(modifiers), false)));
		}
		if ((modifiers & ACC_SKILL) != 0) {
			return this.imageHelper.getImage(this.images.forSkill(toJVMVisibility(modifiers),
					toAdornments(removeFlags(modifiers), true)));
		}
		if ((modifiers & ACC_EVENT) != 0) {
			return this.imageHelper.getImage(this.images.forEvent(toJVMVisibility(modifiers),
					toAdornments(removeFlags(modifiers), true)));
		}
		return super.computeImage(typeName, isInnerType, removeFlags(modifiers));
	}

}
