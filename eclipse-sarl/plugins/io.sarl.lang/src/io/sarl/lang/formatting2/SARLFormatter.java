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

package io.sarl.lang.formatting2;

import java.text.MessageFormat;
import java.util.Collection;

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.formatting2.XtendFormatter;
import org.eclipse.xtend.core.formatting2.XtendFormatterPreferenceKeys;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnumLiteral;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.formatting2.FormattingNotApplicableException;
import org.eclipse.xtext.formatting2.IFormattableDocument;
import org.eclipse.xtext.formatting2.IHiddenRegionFormatter;
import org.eclipse.xtext.formatting2.ITextReplacer;
import org.eclipse.xtext.formatting2.internal.CommentReplacer;
import org.eclipse.xtext.formatting2.regionaccess.IComment;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegion;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegionFinder;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegionsFinder;
import org.eclipse.xtext.grammaranalysis.impl.GrammarElementTitleSwitch;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationElementValuePair;
import org.eclipse.xtext.xbase.formatting2.XbaseFormatterPreferenceKeys;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;

/**
 * This class contains custom formatting description.
 *
 * <p>Developers: for avoiding formatting conflicts between two keywords, try to avoid "surounding" and
 * use only "prepend".
 *
 * <p>The {@link FormatterFacade} provides a convinient API for formatting strings.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see FormatterFacade
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class SARLFormatter extends XtendFormatter {

	/** Name to use for injected comments.
	 */
	public static final String COMMENT_NAME = "io.sarl.lang.formatting2.COMMENT_TO_FORMAT"; //$NON-NLS-1$

	/** Name to use for injected comment prefix.
	 */
	public static final String COMMENT_PREFIX_NAME = "io.sarl.lang.formatting2.COMMENT_PREFIX"; //$NON-NLS-1$

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

	private static final Procedure1<IHiddenRegionFormatter> INDENT = new Procedure1<IHiddenRegionFormatter>() {
		@Override
		public void apply(IHiddenRegionFormatter it) {
			it.indent();
		}
	};

	@Inject
	private Injector injector;

	@Override
	public ITextReplacer createCommentReplacer(IComment comment) {
		final EObject grammarElement = comment.getGrammarElement();
		if (grammarElement instanceof AbstractRule) {
			final String ruleName = ((AbstractRule) grammarElement).getName();
			CommentReplacer replacer = null;
			if (ruleName.startsWith("ML")) { //$NON-NLS-1$
				replacer = new SARLMultilineCommentReplacer(comment);
			} else if (ruleName.startsWith("SL")) { //$NON-NLS-1$
				replacer = new SARLSinglelineCommentReplacer(comment);
			}
			if (replacer != null) {
				this.injector.injectMembers(replacer);
				return replacer;
			}
		}
		final String elementName = new GrammarElementTitleSwitch().showQualified().showRule().doSwitch(grammarElement);
		throw new IllegalStateException(
				MessageFormat.format(Messages.SARLFormatter_0,
						ITextReplacer.class.getSimpleName(), elementName));
	}

	@Override
	public void format(Object sarlComponent, IFormattableDocument document) {
		if (sarlComponent instanceof SarlEvent) {
			_format((SarlEvent) sarlComponent, document);
		} else if (sarlComponent instanceof SarlCapacity) {
			_format((SarlCapacity) sarlComponent, document);
		} else if (sarlComponent instanceof SarlAgent) {
			_format((SarlAgent) sarlComponent, document);
		} else if (sarlComponent instanceof SarlBehavior) {
			_format((SarlBehavior) sarlComponent, document);
		} else if (sarlComponent instanceof SarlSkill) {
			_format((SarlSkill) sarlComponent, document);
		} else if (sarlComponent instanceof SarlBehaviorUnit) {
			_format((SarlBehaviorUnit) sarlComponent, document);
		} else if (sarlComponent instanceof SarlCapacityUses) {
			_format((SarlCapacityUses) sarlComponent, document);
		} else if (sarlComponent instanceof SarlRequiredCapacity) {
			_format((SarlRequiredCapacity) sarlComponent, document);
		} else {
			try {
				super.format(sarlComponent, document);
			} catch (FormattingNotApplicableException exception) {
				// Silently ignore this exception.
			}
		}
	}

	@Override
	protected void _format(XBlockExpression expr, IFormattableDocument document) {
		if (getPreferences().getPreference(SARLFormatterPreferenceKeys.ENABLE_SINGLELINE_EXPRESSION).booleanValue()) {
			super._format(expr, document);
		} else {
			// Avoid to format the block expression on a single line.
			final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(expr);
			if (expr.eContainer() == null) {
				document.surround(expr, NO_SPACE);
			}
			final ISemanticRegion open = regionFor.keyword("{"); //$NON-NLS-1$
			final ISemanticRegion close = regionFor.keyword("}"); //$NON-NLS-1$
			if (open != null && close != null) {
				formatExpressionsMultiline(expr.getExpressions(), open, close, document);
			}
		}
	}

	/** Format the given SARL event.
	 *
	 * @param event the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlEvent event, IFormattableDocument document) {
		formatAnnotations(event, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(event, document);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(event);
		document.append(regionFor.keyword("event"), ONE_SPACE); //$NON-NLS-1$

		document.surround(regionFor.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(event.getExtends());

		formatBody(event, document);
	}

	/** Format the given SARL capacity.
	 *
	 * @param capacity the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlCapacity capacity, IFormattableDocument document) {
		formatAnnotations(capacity, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(capacity, document);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(capacity);

		document.append(regionFor.keyword("capacity"), ONE_SPACE); //$NON-NLS-1$

		document.surround(regionFor.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		formatCommaSeparatedList(capacity.getExtends(), document);

		formatBody(capacity, document);
	}

	/** Format the given SARL agent.
	 *
	 * @param agent the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlAgent agent, IFormattableDocument document) {
		formatAnnotations(agent, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(agent, document);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(agent);
		document.append(regionFor.keyword("agent"), ONE_SPACE); //$NON-NLS-1$

		document.surround(regionFor.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(agent.getExtends());

		formatBody(agent, document);
	}

	/** Format the given SARL behavior.
	 *
	 * @param behavior the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlBehavior behavior, IFormattableDocument document) {
		formatAnnotations(behavior, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(behavior, document);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(behavior);
		document.append(regionFor.keyword("behavior"), ONE_SPACE); //$NON-NLS-1$

		document.surround(regionFor.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(behavior.getExtends());

		formatBody(behavior, document);
	}

	/** Format the given SARL skill.
	 *
	 * @param skill the SARL component.
	 * @param document the document.
	 */
	protected void _format(SarlSkill skill, IFormattableDocument document) {
		formatAnnotations(skill, document, XbaseFormatterPreferenceKeys.newLineAfterClassAnnotations);
		formatModifiers(skill, document);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(skill);
		document.append(regionFor.keyword("skill"), ONE_SPACE); //$NON-NLS-1$

		document.surround(regionFor.keyword("extends"), ONE_SPACE); //$NON-NLS-1$
		document.format(skill.getExtends());

		document.surround(regionFor.keyword("implements"), ONE_SPACE); //$NON-NLS-1$
		formatCommaSeparatedList(skill.getImplements(), document);

		formatBody(skill, document);
	}

	@Override
	protected void _format(XtendField field, IFormattableDocument document) {
		formatAnnotations(field, document, XbaseFormatterPreferenceKeys.newLineAfterFieldAnnotations);
		formatModifiers(field, document);
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(field);

		final ISemanticRegion columnKw = regionFor.keyword(":"); //$NON-NLS-1$
		document.prepend(columnKw, ONE_SPACE);
		document.append(columnKw, ONE_SPACE);
		final ISemanticRegion equalKw = regionFor.keyword("="); //$NON-NLS-1$
		document.prepend(equalKw, ONE_SPACE);
		document.append(equalKw, ONE_SPACE);
		final ISemanticRegion semicolumn = regionFor.keyword(";"); //$NON-NLS-1$
		document.prepend(semicolumn, NO_SPACE);

		final JvmTypeReference type = field.getType();
		document.format(type);
		final XExpression initialValue = field.getInitialValue();
		document.format(initialValue);
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected void _format(XtendFunction function, IFormattableDocument document) {
		formatAnnotations(function, document, XbaseFormatterPreferenceKeys.newLineAfterMethodAnnotations);
		formatModifiers(function, document);

		final EList<JvmTypeParameter> typeParameters = function.getTypeParameters();
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(function);

		if (!typeParameters.isEmpty()) {
			final ISemanticRegion open = regionFor.keyword("<"); //$NON-NLS-1$
			document.prepend(open, ONE_SPACE);
			document.append(open, NO_SPACE);
			final ISemanticRegion close = regionFor.keyword(">"); //$NON-NLS-1$
			document.prepend(close, NO_SPACE);
			document.append(close, ONE_SPACE);
			document.surround(regionFor.keyword("with"), ONE_SPACE); //$NON-NLS-1$
			formatCommaSeparatedList(function.getTypeParameters(), document);
		}

		final ISemanticRegion nameNode = regionFor.feature(XtendPackage.Literals.XTEND_FUNCTION__NAME);
		final ISemanticRegionFinder immediatelyFollowing;
		if (nameNode != null) {
			immediatelyFollowing = nameNode.immediatelyFollowing();
		} else {
			immediatelyFollowing = null;
		}
		final ISemanticRegion open;
		if (immediatelyFollowing != null) {
			open = immediatelyFollowing.keyword("("); //$NON-NLS-1$
		} else {
			open = null;
		}
		final ISemanticRegion close = regionFor.keyword(")"); //$NON-NLS-1$

		final JvmTypeReference returnType = function.getReturnType();
		if (returnType != null) {
			final ISemanticRegion typeColumn = this.textRegionExtensions.immediatelyPreceding(returnType)
					.keyword(":"); //$NON-NLS-1$
			document.surround(typeColumn, ONE_SPACE);
			document.format(returnType);
		}

		final XExpression expression = function.getExpression();
		if (expression != null) {
			final ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(expression);
			final ISemanticRegion brace = finder.keyword("{"); //$NON-NLS-1$
			document.prepend(brace, XbaseFormatterPreferenceKeys.bracesInNewLine);
			document.format(expression);
		} else {
			final ISemanticRegion column = regionFor.keyword(";"); //$NON-NLS-1$
			document.prepend(column, NO_SPACE);
		}

		final EList<XtendParameter> parameters = function.getParameters();
		formatCommaSeparatedList(parameters, open, close, document);

		final EList<JvmTypeReference> exceptions = function.getExceptions();
		if (!exceptions.isEmpty()) {
			document.surround(regionFor.keyword("throws"), ONE_SPACE); //$NON-NLS-1$
		}
		formatCommaSeparatedList(exceptions, document);

		if (function instanceof SarlAction) {
			final EList<JvmTypeReference> events = ((SarlAction) function).getFiredEvents();
			if (!events.isEmpty()) {
				document.surround(regionFor.keyword("fires"), ONE_SPACE); //$NON-NLS-1$
			}
			formatCommaSeparatedList(events, document);
		}
	}

	@Override
	protected void _format(XtendConstructor constructor, IFormattableDocument document) {
		formatAnnotations(constructor, document, XbaseFormatterPreferenceKeys.newLineAfterConstructorAnnotations);
		formatModifiers(constructor, document);

		final EList<JvmTypeParameter> typeParameters = constructor.getTypeParameters();
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(constructor);

		final ISemanticRegion nameNode = regionFor.keyword("new"); //$NON-NLS-1$
		final ISemanticRegionFinder immediatelyFollowing;

		if (!typeParameters.isEmpty()) {
			final ISemanticRegion open = regionFor.keyword("<"); //$NON-NLS-1$
			document.prepend(open, ONE_SPACE);
			document.append(open, NO_SPACE);
			final ISemanticRegion close = regionFor.keyword(">"); //$NON-NLS-1$
			document.prepend(close, NO_SPACE);
			document.append(close, ONE_SPACE);
			document.surround(regionFor.keyword("with"), ONE_SPACE); //$NON-NLS-1$
			formatCommaSeparatedList(constructor.getTypeParameters(), document);
			if (close != null) {
				immediatelyFollowing = close.immediatelyFollowing();
			} else if (nameNode != null) {
				immediatelyFollowing = nameNode.immediatelyFollowing();
			} else {
				immediatelyFollowing = null;
			}
		} else if (nameNode != null) {
			immediatelyFollowing = nameNode.immediatelyFollowing();
		} else {
			immediatelyFollowing = null;
		}

		final ISemanticRegion open;
		if (immediatelyFollowing != null) {
			open = immediatelyFollowing.keyword("("); //$NON-NLS-1$
		} else {
			open = null;
		}
		final ISemanticRegion close = regionFor.keyword(")"); //$NON-NLS-1$

		final XExpression expression = constructor.getExpression();
		if (expression != null) {
			final ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(expression);
			final ISemanticRegion brace = finder.keyword("{"); //$NON-NLS-1$
			document.prepend(brace, XbaseFormatterPreferenceKeys.bracesInNewLine);
			document.format(expression);
		}

		final EList<XtendParameter> parameters = constructor.getParameters();
		formatCommaSeparatedList(parameters, open, close, document);

		final EList<JvmTypeReference> exceptions = constructor.getExceptions();
		if (!exceptions.isEmpty()) {
			document.surround(regionFor.keyword("throws"), ONE_SPACE); //$NON-NLS-1$
		}
		formatCommaSeparatedList(exceptions, document);
	}

	/** Format a behavior unit.
	 *
	 * @param behaviorUnit the behavior unit.
	 * @param document the document.
	 */
	protected void _format(SarlBehaviorUnit behaviorUnit, IFormattableDocument document) {
		formatAnnotations(behaviorUnit, document, XbaseFormatterPreferenceKeys.newLineAfterMethodAnnotations);

		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(behaviorUnit);

		document.append(regionFor.keyword("on"), ONE_SPACE); //$NON-NLS-1$

		if (behaviorUnit.getGuard() != null) {
			ISemanticRegion keyword = this.textRegionExtensions.immediatelyPreceding(
					behaviorUnit.getGuard()).keyword("["); //$NON-NLS-1$
			document.prepend(keyword, ONE_SPACE);
			document.append(keyword, NO_SPACE);
			keyword = this.textRegionExtensions.immediatelyFollowing(
					behaviorUnit.getGuard()).keyword("]"); //$NON-NLS-1$
			document.prepend(keyword, NO_SPACE);
		}

		document.format(behaviorUnit.getName());
		document.format(behaviorUnit.getGuard());

		final XExpression expression = behaviorUnit.getExpression();
		if (expression != null) {
			final ISemanticRegionFinder finder = this.textRegionExtensions.regionFor(expression);
			final ISemanticRegion brace = finder.keyword("{"); //$NON-NLS-1$
			document.prepend(brace, XbaseFormatterPreferenceKeys.bracesInNewLine);
			document.format(expression);
		}
	}

	/** Format a capacity use.
	 *
	 * @param capacityUses the capacity uses.
	 * @param document the document.
	 */
	protected void _format(SarlCapacityUses capacityUses, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(capacityUses);
		document.append(regionFor.keyword("uses"), ONE_SPACE); //$NON-NLS-1$
		formatCommaSeparatedList(capacityUses.getCapacities(), document);
		document.prepend(regionFor.keyword(";"), NO_SPACE); //$NON-NLS-1$
	}

	/** Format a required capacity.
	 *
	 * @param requiredCapacity the element ot format.
	 * @param document the document.
	 */
	protected void _format(SarlRequiredCapacity requiredCapacity, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(requiredCapacity);
		document.append(regionFor.keyword("requires"), ONE_SPACE); //$NON-NLS-1$
		formatCommaSeparatedList(requiredCapacity.getCapacities(), document);
		document.prepend(regionFor.keyword(";"), NO_SPACE); //$NON-NLS-1$
	}

	@Override
	protected void _format(XtendParameter param, IFormattableDocument document) {
		formatAnnotations(param, document, XbaseFormatterPreferenceKeys.newLineAfterParameterAnnotations);
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(param);
		document.surround(regionFor.keyword(":"), ONE_SPACE); //$NON-NLS-1$
		document.surround(regionFor.keyword("="), ONE_SPACE); //$NON-NLS-1$
		final JvmTypeReference parameterType = param.getParameterType();
		if (parameterType != null) {
			document.format(parameterType);
			final ISemanticRegion varArgToken = regionFor.keyword("*"); //$NON-NLS-1$
			document.surround(varArgToken, NO_SPACE);
		}
	}

	@Override
	protected void _format(XVariableDeclaration expr, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword("extension"), ONE_SPACE); //$NON-NLS-1$
		document.append(regionFor.keyword("val"), ONE_SPACE); //$NON-NLS-1$
		document.append(regionFor.keyword("var"), ONE_SPACE); //$NON-NLS-1$
		document.surround(regionFor.keyword(":"), ONE_SPACE); //$NON-NLS-1$
		document.surround(regionFor.keyword("="), ONE_SPACE); //$NON-NLS-1$
		document.format(expr.getType());
		document.format(expr.getRight());
	}

	@Override
	protected void _format(JvmFormalParameter expr, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword("extension"), ONE_SPACE); //$NON-NLS-1$
		final JvmTypeReference parameterType = expr.getParameterType();
		if (parameterType != null) {
			document.surround(regionFor.keyword(":"), ONE_SPACE); //$NON-NLS-1$
			document.surround(regionFor.keyword("as"), ONE_SPACE); //$NON-NLS-1$
		}
		document.format(parameterType);
	}

	@Override
	protected void _format(XForLoopExpression expr, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword("for"), ONE_SPACE); //$NON-NLS-1$

		final JvmFormalParameter declaredParam = expr.getDeclaredParam();
		document.prepend(declaredParam, NO_SPACE);
		document.append(declaredParam, ONE_SPACE);
		document.format(declaredParam);

		final XExpression forExpression = expr.getForExpression();
		document.prepend(forExpression, ONE_SPACE);
		document.append(forExpression, NO_SPACE);
		document.format(forExpression);

		final XExpression eachExpression = expr.getEachExpression();
		if (eachExpression != null) {
			formatBody(eachExpression, true, document);
		} else {
			document.prepend(regionFor.keyword(";"), NO_SPACE); //$NON-NLS-1$
		}
	}

	@Override
	protected void _format(XAnnotation annotation, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(annotation);
		document.append(regionFor.keyword("@"), NO_SPACE); //$NON-NLS-1$
		document.surround(regionFor.keyword("("), NO_SPACE); //$NON-NLS-1$
		XExpression value = annotation.getValue();
		if (value != null) {
			document.format(value);
		} else {
			final EList<XAnnotationElementValuePair> elementValuePairs = annotation.getElementValuePairs();
			if (!elementValuePairs.isEmpty()) {
				for (final XAnnotationElementValuePair pair : elementValuePairs) {
					final ISemanticRegionsFinder regionForPair = this.textRegionExtensions.regionFor(pair);
					document.surround(regionForPair.keyword("="), ONE_SPACE); //$NON-NLS-1$
					value = pair.getValue();
					document.format(value);
					final ISemanticRegionFinder immediatelyFollowing = this.textRegionExtensions.immediatelyFollowing(pair);
					final ISemanticRegion keyword = immediatelyFollowing.keyword(","); //$NON-NLS-1$
					document.prepend(keyword, NO_SPACE);
					document.append(keyword, ONE_SPACE);
				}
			}
		}
		document.prepend(regionFor.keyword(")"), NO_SPACE); //$NON-NLS-1$
	}

	@Override
	protected void _format(Object element, IFormattableDocument document) {
		throw new UnsupportedOperationException(element.getClass().getName());
	}

	/** Format a list of comma separated elements.
	 *
	 * <p>This function does not considerer opening and closing delimiters, as
	 * {@link #formatCommaSeparatedList(Collection, ISemanticRegion, ISemanticRegion, IFormattableDocument)}.
	 *
	 * @param elements the elements to format.
	 * @param document the document.
	 */
	protected void formatCommaSeparatedList(Collection<? extends EObject> elements, IFormattableDocument document) {
		for (final EObject element : elements) {
			document.format(element);
			final ISemanticRegionFinder immediatelyFollowing = this.textRegionExtensions.immediatelyFollowing(element);
			final ISemanticRegion keyword = immediatelyFollowing.keyword(","); //$NON-NLS-1$
			document.prepend(keyword, NO_SPACE);
			document.append(keyword, ONE_SPACE);
		}
	}

	@Override
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	protected ISemanticRegion formatBody(XtendTypeDeclaration type, IFormattableDocument document) {
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(type);

		final ISemanticRegion open = regionFor.keyword("{"); //$NON-NLS-1$
		final ISemanticRegion close = regionFor.keyword("}"); //$NON-NLS-1$
		document.prepend(open, XbaseFormatterPreferenceKeys.bracesInNewLine);
		document.interior(open, close, INDENT);

		final EList<XtendMember> members = type.getMembers();
		if (!members.isEmpty()) {
			XtendMember previous = null;
			for (final XtendMember current : members) {
				document.format(current);
				if (previous == null) {
					document.prepend(current, XtendFormatterPreferenceKeys.blankLinesBeforeFirstMember);
				} else if (previous instanceof XtendField) {
					if (current instanceof XtendField) {
						document.append(previous, XtendFormatterPreferenceKeys.blankLinesBetweenFields);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else if (previous instanceof XtendExecutable) {
					if (current instanceof XtendExecutable) {
						document.append(previous, XtendFormatterPreferenceKeys.blankLinesBetweenMethods);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else if (previous instanceof XtendTypeDeclaration) {
					if (current instanceof XtendTypeDeclaration) {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_INNER_TYPES);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else if (previous instanceof XtendEnumLiteral) {
					if (current instanceof XtendEnumLiteral) {
						document.append(previous, XtendFormatterPreferenceKeys.blankLinesBetweenEnumLiterals);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else if (previous instanceof SarlCapacityUses) {
					if (current instanceof SarlCapacityUses) {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_CAPACITY_USES);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else if (previous instanceof SarlRequiredCapacity) {
					if (current instanceof SarlCapacityUses) {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_CAPACITY_REQUIREMENTS);
					} else {
						document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
					}
				} else {
					document.append(previous, SARLFormatterPreferenceKeys.BLANK_LINES_BETWEEN_MEMBER_CATEGORIES);
				}
				previous = current;
			}
			if (previous != null) {
				document.append(previous, XtendFormatterPreferenceKeys.blankLinesAfterLastMember);
			}
			return null;
		}
		return document.append(open, NEW_LINE);
	}

}
