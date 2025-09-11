/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import com.google.inject.Inject;
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
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
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
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * This class contains custom formatting description.
 *
 * <p>Developers: for avoiding formatting conflicts between two keywords, try to avoid "surounding" and
 * use only "prepend".
 *
 * <p>The {@link FormatterFacade} provides a convinient API for formatting strings.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @see FormatterFacade
 */
public class SARLFormatter extends XtendFormatter {

	/** Name to use for injected comments.
	 */
	public static final String COMMENT_NAME = "io.sarl.lang.formatting2.COMMENT_TO_FORMAT"; //$NON-NLS-1$

	/** Name to use for injected comment prefix.
	 */
	public static final String COMMENT_PREFIX_NAME = "io.sarl.lang.formatting2.COMMENT_PREFIX"; //$NON-NLS-1$

	private static final Procedure1<? super IHiddenRegionFormatter> ONE_SPACE = it -> {
		it.oneSpace();
	};

	private static final Procedure1<? super IHiddenRegionFormatter> NO_SPACE = it -> {
		it.noSpace();
	};

	private static final Procedure1<? super IHiddenRegionFormatter> NEW_LINE = it -> {
		it.newLine();
	};

	private static final Procedure1<? super IHiddenRegionFormatter> INDENT = it -> {
		it.indent();
	};

	@Inject
	private Injector injector;

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Override
	public ITextReplacer createCommentReplacer(IComment comment) {
		final var grammarElement = comment.getGrammarElement();
		if (grammarElement instanceof AbstractRule cvalue) {
			final var ruleName = cvalue.getName();
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
		final var elementName = new GrammarElementTitleSwitch().showQualified().showRule().doSwitch(grammarElement);
		throw new IllegalStateException(
				MessageFormat.format(Messages.SARLFormatter_0,
						ITextReplacer.class.getSimpleName(), elementName));
	}

	@Override
	public void format(Object sarlComponent, IFormattableDocument document) {
		try {
			if (sarlComponent instanceof SarlEvent cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlCapacity cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlAgent cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlBehavior cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlSkill cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlBehaviorUnit cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlCapacityUses cvalue) {
				_format(cvalue, document);
			} else if (sarlComponent instanceof SarlRequiredCapacity cvalue) {
				_format(cvalue, document);
			} else {
				super.format(sarlComponent, document);
			}
		} catch (Throwable exception) {
			// Silently ignore this exception.
		}
	}

	@Override
	protected void _format(XBlockExpression expr, IFormattableDocument document) {
		if (getPreferences().getPreference(SARLFormatterPreferenceKeys.ENABLE_SINGLELINE_EXPRESSION).booleanValue()) {
			super._format(expr, document);
		} else {
			// Avoid to format the block expression on a single line.
			final var regionFor = this.textRegionExtensions.regionFor(expr);
			if (expr.eContainer() == null) {
				document.surround(expr, NO_SPACE);
			}
			final var open = regionFor.keyword(this.keywords.getLeftCurlyBracketKeyword());
			final var close = regionFor.keyword(this.keywords.getRightCurlyBracketKeyword());
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
		formatTypeParameters(event, event.getTypeParameters(), document);

		final var regionFor = this.textRegionExtensions.regionFor(event);
		document.append(regionFor.keyword(this.keywords.getEventKeyword()), ONE_SPACE);

		document.surround(regionFor.keyword(this.keywords.getExtendsKeyword()), ONE_SPACE);
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

		final var regionFor = this.textRegionExtensions.regionFor(capacity);

		document.append(regionFor.keyword(this.keywords.getCapacityKeyword()), ONE_SPACE);

		document.surround(regionFor.keyword(this.keywords.getExtendsKeyword()), ONE_SPACE);
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

		final var regionFor = this.textRegionExtensions.regionFor(agent);
		document.append(regionFor.keyword(this.keywords.getAgentKeyword()), ONE_SPACE);

		document.surround(regionFor.keyword(this.keywords.getExtendsKeyword()), ONE_SPACE);
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

		final var regionFor = this.textRegionExtensions.regionFor(behavior);
		document.append(regionFor.keyword(this.keywords.getBehaviorKeyword()), ONE_SPACE);

		document.surround(regionFor.keyword(this.keywords.getExtendsKeyword()), ONE_SPACE);
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

		final var regionFor = this.textRegionExtensions.regionFor(skill);
		document.append(regionFor.keyword(this.keywords.getSkillKeyword()), ONE_SPACE);

		document.surround(regionFor.keyword(this.keywords.getExtendsKeyword()), ONE_SPACE);
		document.format(skill.getExtends());

		document.surround(regionFor.keyword(this.keywords.getImplementsKeyword()), ONE_SPACE);
		formatCommaSeparatedList(skill.getImplements(), document);

		formatBody(skill, document);
	}

	@Override
	protected void _format(XtendField field, IFormattableDocument document) {
		formatAnnotations(field, document, XbaseFormatterPreferenceKeys.newLineAfterFieldAnnotations);
		formatModifiers(field, document);
		final var regionFor = this.textRegionExtensions.regionFor(field);

		final var columnKw = regionFor.keyword(this.keywords.getColonKeyword());
		document.prepend(columnKw, ONE_SPACE);
		document.append(columnKw, ONE_SPACE);
		final var equalKw = regionFor.keyword(this.keywords.getEqualsSignKeyword());
		document.prepend(equalKw, ONE_SPACE);
		document.append(equalKw, ONE_SPACE);
		final var semicolumn = regionFor.keyword(this.keywords.getSemicolonKeyword());
		document.prepend(semicolumn, NO_SPACE);

		final var type = field.getType();
		document.format(type);
		final var initialValue = field.getInitialValue();
		document.format(initialValue);
	}

	@Override
	protected void _format(XtendFunction function, IFormattableDocument document) {
		formatAnnotations(function, document, XbaseFormatterPreferenceKeys.newLineAfterMethodAnnotations);
		formatModifiers(function, document);

		final var typeParameters = function.getTypeParameters();
		final var regionFor = this.textRegionExtensions.regionFor(function);

		if (!typeParameters.isEmpty()) {
			final var open = regionFor.keyword(this.keywords.getLessThanSignKeyword());
			document.prepend(open, ONE_SPACE);
			document.append(open, NO_SPACE);
			final var close = regionFor.keyword(this.keywords.getGreaterThanSignKeyword());
			document.prepend(close, NO_SPACE);
			document.append(close, ONE_SPACE);
			document.surround(regionFor.keyword(this.keywords.getWithKeyword()), ONE_SPACE);
			formatCommaSeparatedList(function.getTypeParameters(), document);
		}

		final var nameNode = regionFor.feature(XtendPackage.Literals.XTEND_FUNCTION__NAME);
		final ISemanticRegionFinder immediatelyFollowing;
		if (nameNode != null) {
			immediatelyFollowing = nameNode.immediatelyFollowing();
		} else {
			immediatelyFollowing = null;
		}
		final ISemanticRegion open;
		if (immediatelyFollowing != null) {
			open = immediatelyFollowing.keyword(this.keywords.getLeftParenthesisKeyword());
			document.prepend(open, NO_SPACE);
			final var close = regionFor.keyword(this.keywords.getRightParenthesisKeyword());
			final var parameters = function.getParameters();
			formatCommaSeparatedList(parameters, open, close, document);
		} else {
			open = null;
		}

		final var returnType = function.getReturnType();
		if (returnType != null) {
			final var typeColumn = this.textRegionExtensions.immediatelyPreceding(returnType)
					.keyword(this.keywords.getColonKeyword());
			document.surround(typeColumn, ONE_SPACE);
			document.format(returnType);
		}

		final var exceptions = function.getExceptions();
		if (!exceptions.isEmpty()) {
			document.surround(regionFor.keyword(this.keywords.getThrowsKeyword()), ONE_SPACE);
		}
		formatCommaSeparatedList(exceptions, document);

		if (function instanceof SarlAction cvalue) {
			final var events = cvalue.getFiredEvents();
			if (!events.isEmpty()) {
				document.surround(regionFor.keyword(this.keywords.getFiresKeyword()), ONE_SPACE);
			}
			formatCommaSeparatedList(events, document);
		}

		final var expression = function.getExpression();
		if (expression != null) {
			final var finder = this.textRegionExtensions.regionFor(expression);
			final var brace = finder.keyword(this.keywords.getLeftCurlyBracketKeyword());
			document.prepend(brace, XbaseFormatterPreferenceKeys.bracesInNewLine);
			document.format(expression);
		} else {
			final var column = regionFor.keyword(this.keywords.getSemicolonKeyword());
			document.prepend(column, NO_SPACE);
		}
	}

	@Override
	protected void _format(XtendConstructor constructor, IFormattableDocument document) {
		formatAnnotations(constructor, document, XbaseFormatterPreferenceKeys.newLineAfterConstructorAnnotations);
		formatModifiers(constructor, document);

		final EList<JvmTypeParameter> typeParameters = constructor.getTypeParameters();
		final ISemanticRegionsFinder regionFor = this.textRegionExtensions.regionFor(constructor);

		final var nameNode = regionFor.keyword(this.keywords.getNewKeyword());
		final ISemanticRegionFinder immediatelyFollowing;

		if (!typeParameters.isEmpty()) {
			final var open = regionFor.keyword(this.keywords.getLessThanSignKeyword());
			document.prepend(open, ONE_SPACE);
			document.append(open, NO_SPACE);
			final var close = regionFor.keyword(this.keywords.getGreaterThanSignKeyword());
			document.prepend(close, NO_SPACE);
			document.append(close, ONE_SPACE);
			document.surround(regionFor.keyword(this.keywords.getWithKeyword()), ONE_SPACE);
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
			open = immediatelyFollowing.keyword(this.keywords.getLeftParenthesisKeyword());
		} else {
			open = null;
		}
		final var close = regionFor.keyword(this.keywords.getRightParenthesisKeyword());

		final var expression = constructor.getExpression();
		if (expression != null) {
			final var finder = this.textRegionExtensions.regionFor(expression);
			final var brace = finder.keyword(this.keywords.getLeftCurlyBracketKeyword());
			document.prepend(brace, XbaseFormatterPreferenceKeys.bracesInNewLine);
			document.format(expression);
		}

		final var parameters = constructor.getParameters();
		formatCommaSeparatedList(parameters, open, close, document);

		final var exceptions = constructor.getExceptions();
		if (!exceptions.isEmpty()) {
			document.surround(regionFor.keyword(this.keywords.getThrowsKeyword()), ONE_SPACE);
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

		final var regionFor = this.textRegionExtensions.regionFor(behaviorUnit);

		document.append(regionFor.keyword(this.keywords.getOnKeyword()), ONE_SPACE);

		if (behaviorUnit.getGuard() != null) {
			var keyword = this.textRegionExtensions.immediatelyPreceding(
					behaviorUnit.getGuard()).keyword(this.keywords.getLeftSquareBracketKeyword());
			document.prepend(keyword, ONE_SPACE);
			document.append(keyword, NO_SPACE);
			keyword = this.textRegionExtensions.immediatelyFollowing(
					behaviorUnit.getGuard()).keyword(this.keywords.getRightSquareBracketKeyword());
			document.prepend(keyword, NO_SPACE);
		}

		document.format(behaviorUnit.getName());
		document.format(behaviorUnit.getGuard());

		final var expression = behaviorUnit.getExpression();
		if (expression != null) {
			final var finder = this.textRegionExtensions.regionFor(expression);
			final var brace = finder.keyword(this.keywords.getLeftCurlyBracketKeyword());
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
		final var regionFor = this.textRegionExtensions.regionFor(capacityUses);
		document.append(regionFor.keyword(this.keywords.getUsesKeyword()), ONE_SPACE);
		formatCommaSeparatedList(capacityUses.getCapacities(), document);
		document.prepend(regionFor.keyword(this.keywords.getSemicolonKeyword()), NO_SPACE);
	}

	/** Format a required capacity.
	 *
	 * @param requiredCapacity the element ot format.
	 * @param document the document.
	 */
	protected void _format(SarlRequiredCapacity requiredCapacity, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(requiredCapacity);
		document.append(regionFor.keyword(this.keywords.getRequiresKeyword()), ONE_SPACE);
		formatCommaSeparatedList(requiredCapacity.getCapacities(), document);
		document.prepend(regionFor.keyword(this.keywords.getSemicolonKeyword()), NO_SPACE);
	}

	@Override
	protected void _format(XtendParameter param, IFormattableDocument document) {
		formatAnnotations(param, document, XbaseFormatterPreferenceKeys.newLineAfterParameterAnnotations);
		final var regionFor = this.textRegionExtensions.regionFor(param);
		document.surround(regionFor.keyword(this.keywords.getColonKeyword()), ONE_SPACE);
		document.surround(regionFor.keyword(this.keywords.getEqualsSignKeyword()), ONE_SPACE);
		final var parameterType = param.getParameterType();
		if (parameterType != null) {
			document.format(parameterType);
			final var varArgToken = regionFor.keyword(this.keywords.getWildcardAsteriskKeyword());
			document.surround(varArgToken, NO_SPACE);
		}
	}

	@Override
	protected void _format(XVariableDeclaration expr, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword(this.keywords.getExtensionExtensionKeyword()), ONE_SPACE);
		document.append(regionFor.keyword(this.keywords.getValKeyword()), ONE_SPACE);
		document.append(regionFor.keyword(this.keywords.getWriteableVarKeyword()), ONE_SPACE);
		document.surround(regionFor.keyword(this.keywords.getColonKeyword()), ONE_SPACE);
		document.surround(regionFor.keyword(this.keywords.getEqualsSignKeyword()), ONE_SPACE);
		document.format(expr.getType());
		document.format(expr.getRight());
	}

	@Override
	protected void _format(JvmFormalParameter expr, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword(this.keywords.getExtensionExtensionKeyword()), ONE_SPACE);
		final var parameterType = expr.getParameterType();
		if (parameterType != null) {
			document.surround(regionFor.keyword(this.keywords.getColonKeyword()), ONE_SPACE);
			document.surround(regionFor.keyword(this.keywords.getAsKeyword()), ONE_SPACE);
		}
		document.format(parameterType);
	}

	@Override
	protected void _format(XForLoopExpression expr, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(expr);
		document.append(regionFor.keyword(this.keywords.getForKeyword()), ONE_SPACE);

		final var declaredParam = expr.getDeclaredParam();
		document.prepend(declaredParam, NO_SPACE);
		document.append(declaredParam, ONE_SPACE);
		document.format(declaredParam);

		final var forExpression = expr.getForExpression();
		document.prepend(forExpression, ONE_SPACE);
		document.append(forExpression, NO_SPACE);
		document.format(forExpression);

		final var eachExpression = expr.getEachExpression();
		if (eachExpression != null) {
			formatBody(eachExpression, true, document);
		} else {
			document.prepend(regionFor.keyword(this.keywords.getSemicolonKeyword()), NO_SPACE);
		}
	}

	@Override
	protected void _format(XAnnotation annotation, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(annotation);
		document.append(regionFor.keyword(this.keywords.getCommercialAtKeyword()), NO_SPACE);
		document.surround(regionFor.keyword(this.keywords.getLeftParenthesisKeyword()), NO_SPACE);
		var value = annotation.getValue();
		if (value != null) {
			document.format(value);
		} else {
			final var elementValuePairs = annotation.getElementValuePairs();
			if (!elementValuePairs.isEmpty()) {
				for (final var pair : elementValuePairs) {
					final var regionForPair = this.textRegionExtensions.regionFor(pair);
					document.surround(regionForPair.keyword(this.keywords.getEqualsSignKeyword()), ONE_SPACE);
					value = pair.getValue();
					document.format(value);
					final var immediatelyFollowing = this.textRegionExtensions.immediatelyFollowing(pair);
					final var keyword = immediatelyFollowing.keyword(this.keywords.getCommaKeyword());
					document.prepend(keyword, NO_SPACE);
					document.append(keyword, ONE_SPACE);
				}
			}
		}
		document.prepend(regionFor.keyword(this.keywords.getRightParenthesisKeyword()), NO_SPACE);
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
		for (final var element : elements) {
			document.format(element);
			final var immediatelyFollowing = this.textRegionExtensions.immediatelyFollowing(element);
			final var keyword = immediatelyFollowing.keyword(this.keywords.getCommaKeyword());
			document.prepend(keyword, NO_SPACE);
			document.append(keyword, ONE_SPACE);
		}
	}

	@Override
	protected ISemanticRegion formatBody(XtendTypeDeclaration type, IFormattableDocument document) {
		final var regionFor = this.textRegionExtensions.regionFor(type);

		final var open = regionFor.keyword(this.keywords.getLeftCurlyBracketKeyword());
		final var close = regionFor.keyword(this.keywords.getRightCurlyBracketKeyword());
		document.prepend(open, XbaseFormatterPreferenceKeys.bracesInNewLine);
		document.interior(open, close, INDENT);

		final var members = type.getMembers();
		if (!members.isEmpty()) {
			XtendMember previous = null;
			for (final var current : members) {
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
