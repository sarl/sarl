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

package io.sarl.lang.formatting2;

import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;

import org.eclipse.xtend.core.formatting2.XtendFormatter;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.formatting2.IFormattableDocument;
import org.eclipse.xtext.formatting2.IHiddenRegionFormatter;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegion;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegionFinder;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegionsFinder;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XSwitchExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.formatting2.NewLineOrPreserveKey;
import org.eclipse.xtext.xbase.formatting2.XbaseFormatterPreferenceKeys;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;

/**
 * This class contains custom formatting description.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */

public class SARLFormatter extends XtendFormatter {

	private static final Procedure1<IHiddenRegionFormatter> ONE_SPACE = new Procedure1<IHiddenRegionFormatter>() {
		@Override
		public void apply(IHiddenRegionFormatter it) {
			it.oneSpace();
		}
	};

	private static final Procedure1<IHiddenRegionFormatter> NO_SPACE = new Procedure1<IHiddenRegionFormatter>() {
		@Override
		public void apply(IHiddenRegionFormatter it) {
			it.noSpace();
		}
	};
	
	private static final Procedure1<IHiddenRegionFormatter> NEW_LINE = new Procedure1<IHiddenRegionFormatter>() {
		@Override
		public void apply(IHiddenRegionFormatter it) {
			it.newLine();
		}
	};

	/** Format a column keyword.
	 *
	 * @param finder the semantic finder of the element to format.
	 * @param document the document to format.
	 */
	@SuppressWarnings("static-method")
	protected void formatColumnKeyword(ISemanticRegionFinder finder, IFormattableDocument document) {
		ISemanticRegion region = finder.keyword(";"); //$NON-NLS-1$
		document.prepend(region, NO_SPACE);
		document.append(region, NEW_LINE);
	}

	@Override
	public void format(Object sarlComponent, IFormattableDocument document) {
		if (sarlComponent instanceof SarlAgent) {
			_format((SarlAgent) sarlComponent, document);
		} else if (sarlComponent instanceof SarlBehavior) {
			_format((SarlBehavior) sarlComponent, document);
		} else if (sarlComponent instanceof SarlCapacity) {
			_format((SarlCapacity) sarlComponent, document);
		} else if (sarlComponent instanceof SarlEvent) {
			_format((SarlEvent) sarlComponent, document);
		} else if (sarlComponent instanceof SarlSkill) {
			_format((SarlSkill) sarlComponent, document);
		} else if (sarlComponent instanceof SarlBehaviorUnit) {
			_format((SarlBehaviorUnit) sarlComponent, document);
		} else if (sarlComponent instanceof SarlCapacityUses) {
			_format((SarlCapacityUses) sarlComponent, document);
		} else if (sarlComponent instanceof SarlRequiredCapacity) {
			_format((SarlRequiredCapacity) sarlComponent, document);
		} else {
			super.format(sarlComponent, document);
		}
	}

	/** Format the agent.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlAgent sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("agent"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(sarlComponent.getExtends());
		formatBody(sarlComponent, document);
	}

	/** Format the behavior.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlBehavior sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("behavior"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(sarlComponent.getExtends());
		formatBody(sarlComponent, document);
	}

	/** Format the capacity.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlCapacity sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("capacity"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		for (JvmTypeReference implementedType : sarlComponent.getExtends()) {
			ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(implementedType).keyword(","); //$NON-NLS-1$
			document.prepend(region, NO_SPACE);
			document.append(region, ONE_SPACE);
			document.format(implementedType);
		}
		formatBody(sarlComponent, document);
	}

	/** Format the event.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlEvent sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("event"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(sarlComponent.getExtends());
		formatBody(sarlComponent, document);
	}

	/** Format the skill.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlSkill sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("skill"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(sarlComponent.getExtends());
		document.surround(finder.keyword("implements"), ONE_SPACE); //$NON-NLS-1$
		for (JvmTypeReference implementedType : sarlComponent.getImplements()) {
			ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(implementedType).keyword(","); //$NON-NLS-1$
			document.prepend(region, NO_SPACE);
			document.append(region, ONE_SPACE);
			document.format(implementedType);
		}
		formatBody(sarlComponent, document);
	}

	/** Format the behavior unit.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlBehaviorUnit sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("on"), ONE_SPACE); //$NON-NLS-1$
		ISemanticRegion braceRegion;
		if (sarlComponent.getGuard() != null) {
			ISemanticRegion region = this.textRegionExtensions.immediatelyPreceding(
					sarlComponent.getGuard()).keyword("["); //$NON-NLS-1$
			document.prepend(region, ONE_SPACE);
			document.append(region, NO_SPACE);
			region = this.textRegionExtensions.immediatelyFollowing(
					sarlComponent.getGuard()).keyword("]"); //$NON-NLS-1$
			document.prepend(region, NO_SPACE);
			document.append(region, ONE_SPACE);
			document.format(sarlComponent.getGuard());
			braceRegion = region.immediatelyFollowing().keyword("{"); //$NON-NLS-1$
		} else {
			braceRegion = this.textRegionExtensions.immediatelyFollowing(
					sarlComponent.getDeclaringType()).keyword("{"); //$NON-NLS-1$
		}
		XExpression expr = sarlComponent.getExpression();
		if (expr != null) {
			document.prepend(braceRegion, XbaseFormatterPreferenceKeys.bracesInNewLine);
		}
		document.format(expr);
		formatColumnKeyword(finder, document);
	}

	/** Format the capacity uses.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlCapacityUses sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("uses"), ONE_SPACE); //$NON-NLS-1$
		for (JvmTypeReference implementedType : sarlComponent.getCapacities()) {
			ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(implementedType).keyword(","); //$NON-NLS-1$
			document.prepend(region, NO_SPACE);
			document.append(region, ONE_SPACE);
			document.format(implementedType);
		}
		formatColumnKeyword(finder, document);
	}

	/** Format the capacity requirements.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlRequiredCapacity sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.append(finder.keyword("requires"), ONE_SPACE); //$NON-NLS-1$
		for (JvmTypeReference implementedType : sarlComponent.getCapacities()) {
			ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(implementedType).keyword(","); //$NON-NLS-1$
			document.prepend(region, NO_SPACE);
			document.append(region, ONE_SPACE);
			document.format(implementedType);
		}
		formatColumnKeyword(finder, document);
	}

	@Override
	protected void _format(XtendField sarlComponent, IFormattableDocument document) {
		formatAnnotations(sarlComponent, document, XbaseFormatterPreferenceKeys.newLineAfterFieldAnnotations);
		formatModifiers(sarlComponent, document);
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		document.surround(finder.keyword(":"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("="), ONE_SPACE); //$NON-NLS-1$
		document.format(sarlComponent.getType());
		document.format(sarlComponent.getInitialValue());
		formatColumnKeyword(finder, document);
	}

	/** Format the executable component of the SARL syntax.
	 *
	 * <p>This function formats the annotations, the modifiers, the parameter list, the exceptions, and the body.
	 *
	 * <p>The type parameters, and the fired events are not formatted by this function.
	 *
	 * @param sarlComponent the SARL component.
	 * @param document the document.
	 * @param finder the finder associated to the SARL component.
	 * @param executableId the region that identify the executable ("new", action name).
	 * @param enableAnnotations indicates if the annotations must be formatted.
	 * @param enableModifiers indicates if the modifiers must be formatted.
	 * @param enableBody indicates if the body must be formatted.
	 */
	protected void formatExecutable(XtendExecutable sarlComponent, IFormattableDocument document,
			ISemanticRegionFinder finder, ISemanticRegion executableId,
			boolean enableAnnotations, boolean enableModifiers, boolean enableBody) {
		if (enableAnnotations) {
			NewLineOrPreserveKey key = (sarlComponent instanceof XtendConstructor)
					? XbaseFormatterPreferenceKeys.newLineAfterConstructorAnnotations
					: XbaseFormatterPreferenceKeys.newLineAfterMethodAnnotations;
			formatAnnotations(sarlComponent, document, key);
		}
		if (enableModifiers) {
			formatModifiers(sarlComponent, document);
		}
		document.append(executableId, NO_SPACE);
		ISemanticRegion open = finder.keyword("("); //$NON-NLS-1$
		document.surround(open, NO_SPACE);
		ISemanticRegion close = finder.keyword(")"); //$NON-NLS-1$
		document.surround(close, NO_SPACE);
		formatCommaSeparatedList(sarlComponent.getParameters(), open, close, document);

		if (!sarlComponent.getExceptions().isEmpty()) {
			document.surround(finder.keyword("throws"), ONE_SPACE); //$NON-NLS-1$
			for (JvmTypeReference type : sarlComponent.getExceptions()) {
				document.format(type);
				ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(type).keyword(","); //$NON-NLS-1$
				document.prepend(region, NO_SPACE);
				document.append(region, ONE_SPACE);
			}
		}

		if (enableBody && sarlComponent.getExpression() != null) {
			document.prepend(
					this.textRegionExtensions.immediatelyPreceding(sarlComponent.getExpression()).keyword("{"), //$NON-NLS-1$
					XbaseFormatterPreferenceKeys.bracesInNewLine);

			document.format(sarlComponent.getExpression());
		}
	}

	@Override
	protected void _format(XtendConstructor sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		ISemanticRegion newKeyword = finder.keyword("new"); //$NON-NLS-1$

		formatExecutable(sarlComponent, document, finder, newKeyword, true, true, true);

		if (!sarlComponent.getTypeParameters().isEmpty()) {
			if (sarlComponent instanceof SarlConstructor) {
				document.surround(finder.keyword("with"), ONE_SPACE); //$NON-NLS-1$
			}
			document.surround(newKeyword.immediatelyFollowing().keyword("<"), NO_SPACE); //$NON-NLS-1$
			ISemanticRegion open = finder.keyword("("); //$NON-NLS-1$
			document.surround(open.immediatelyPreceding().keyword(">"), NO_SPACE); //$NON-NLS-1$
			for (JvmTypeParameter type : sarlComponent.getTypeParameters()) {
				document.format(type);
				ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(type).keyword(","); //$NON-NLS-1$
				document.prepend(region, NO_SPACE);
				document.append(region, ONE_SPACE);
			}
		}
	}

	@Override
	protected void _format(XtendFunction sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
		ISemanticRegion actionName = finder.feature(XTEND_FUNCTION__NAME);

		formatExecutable(sarlComponent, document, finder, actionName, true, true, true);

		if (!sarlComponent.getTypeParameters().isEmpty()) {
			if (sarlComponent instanceof SarlConstructor) {
				document.surround(finder.keyword("with"), ONE_SPACE); //$NON-NLS-1$
			}
			document.surround(actionName.immediatelyPreceding().keyword(">"), NO_SPACE); //$NON-NLS-1$
			document.surround(finder.keyword("<"), NO_SPACE); //$NON-NLS-1$
			for (JvmTypeParameter type : sarlComponent.getTypeParameters()) {
				document.format(type);
				ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(type).keyword(","); //$NON-NLS-1$
				document.prepend(region, NO_SPACE);
				document.append(region, ONE_SPACE);
			}
		}

		if (sarlComponent instanceof SarlAction) {
			SarlAction sarlAction = (SarlAction) sarlComponent;
			if (!sarlAction.getFiredEvents().isEmpty()) {
				document.surround(finder.keyword("fires"), ONE_SPACE); //$NON-NLS-1$
				for (JvmTypeReference type : sarlAction.getFiredEvents()) {
					document.format(type);
					ISemanticRegion region = this.textRegionExtensions.immediatelyFollowing(type).keyword(","); //$NON-NLS-1$
					document.prepend(region, NO_SPACE);
					document.append(region, ONE_SPACE);
				}
			}
		}
		
		formatColumnKeyword(finder, document);
	}

	@Override
	protected void _format(XtendParameter sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);

		if (!sarlComponent.getAnnotations().isEmpty()) {
			for (XAnnotation annotation : sarlComponent.getAnnotations()) {
		        document.format(annotation);
		        document.append(annotation, XbaseFormatterPreferenceKeys.newLineAfterParameterAnnotations);
			}
		}
		
		document.surround(finder.keyword(":"), ONE_SPACE); //$NON-NLS-1$
	
		JvmTypeReference parameterType = sarlComponent.getParameterType();
		if (parameterType != null) {
			document.append(parameterType, ONE_SPACE);
		}
		document.format(parameterType);

		if (sarlComponent instanceof SarlFormalParameter) {
			SarlFormalParameter sarlParameter = (SarlFormalParameter) sarlComponent;
			XExpression defaultValue = sarlParameter.getDefaultValue();
			if (defaultValue != null) {
				document.surround(finder.keyword("="), ONE_SPACE); //$NON-NLS-1$
				document.format(defaultValue);
			}
		}
		
		if (sarlComponent.isVarArg()) {
			document.surround(finder.keyword("*"), NO_SPACE); //$NON-NLS-1$
		}
	}
	
	@Override
	protected void _format(JvmFormalParameter sarlComponent, IFormattableDocument document) {
		ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(sarlComponent);

		if (!sarlComponent.getAnnotations().isEmpty()) {
			for (JvmAnnotationReference annotation : sarlComponent.getAnnotations()) {
		        document.format(annotation);
		        document.append(annotation, XbaseFormatterPreferenceKeys.newLineAfterParameterAnnotations);
			}
		}
		
		document.surround(finder.keyword(":"), ONE_SPACE); //$NON-NLS-1$
		document.surround(finder.keyword("as"), ONE_SPACE); //$NON-NLS-1$

		JvmTypeReference parameterType = sarlComponent.getParameterType();
		if (parameterType != null) {
			document.append(parameterType, ONE_SPACE);
		}
		document.format(parameterType);
	}

	@Override
	protected void _format(XVariableDeclaration sarlComponent, IFormattableDocument document) {
		ISemanticRegionsFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
	    
	    document.append(finder.keyword("val"), ONE_SPACE); //$NON-NLS-1$
	    document.append(finder.keyword("var"), ONE_SPACE); //$NON-NLS-1$
	    document.surround(finder.keyword(":"), ONE_SPACE); //$NON-NLS-1$
	    document.surround(finder.keyword("="), ONE_SPACE); //$NON-NLS-1$

	    document.format(sarlComponent.getType());
	    document.format(sarlComponent.getRight());
	}

	@Override
	protected void _format(XForLoopExpression sarlComponent, IFormattableDocument document) {
	    ISemanticRegionsFinder finder = this.textRegionExtensions.regionFor(sarlComponent);
	    document.append(finder.keyword("for"), ONE_SPACE); //$NON-NLS-1$
	    JvmFormalParameter declaredParam = sarlComponent.getDeclaredParam();
	    document.prepend(declaredParam, NO_SPACE);
	    document.append(declaredParam, ONE_SPACE);
	    document.format(declaredParam);
	    document.surround(finder.keyword(":"), ONE_SPACE); //$NON-NLS-1$
	    XExpression forExpression = sarlComponent.getForExpression();
	    document.append(forExpression, NO_SPACE);
	    document.format(forExpression);
	    formatBody(sarlComponent.getEachExpression(), true, document);
	    formatColumnKeyword(finder, document);
	}

	@Override
	protected void _format(XSwitchExpression sarlComponent, IFormattableDocument document) {
		super._format(sarlComponent, document);
	}

}
