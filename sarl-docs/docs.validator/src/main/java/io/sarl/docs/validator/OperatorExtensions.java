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

package io.sarl.docs.validator;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.eclipse.xtend.core.macro.ProcessorInstanceForJvmTypeProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XInstanceOfExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XPostfixOperation;
import org.eclipse.xtext.xbase.XUnaryOperation;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;

import com.google.inject.Injector;
import com.google.inject.Provider;

import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;

/** Extended Functions for AST support for writing facts within the documentation.
 *
 * @author $Author: sgalland$
 * @version docs.validator 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.validator
 * @since 0.7
 */
public final class OperatorExtensions {

	private static final boolean SHOW_ERROR_ASSOCIATIVITY = false;
	
	private static final boolean SHOW_ERROR_PRECEDENCE = false;

	private static Provider<ParseHelper<SarlScript>> parser;

	private static ValidationTestHelper validation;

	private OperatorExtensions() {
		//
	}

	/** Show the operator precedence table of SARL.
	 *
	 * @param args the arguments (ignored).
	 * @throws Exception in case of error.
	 */
	public static void main(String[] args) throws Exception {
		var operators = Arrays.asList(
				"$v=$i", //$NON-NLS-1$
				"$i||$i", "$i&&$i", //$NON-NLS-1$ //$NON-NLS-2$
				"$i==$i", "$i!=$i", "$i===$i", "$i!==$i", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"$i>=$i", "$i<=$i", "$i<$i", "$i>$i", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"$o instanceof $t", //$NON-NLS-1$
				"$i<=>$i", "$i<>$i", //$NON-NLS-1$ //$NON-NLS-2$
				"$i..$i", "$i>..$i", "$i..<$i", "$i->$i", "$i=>$i", "$i?:$i", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
				"$i>>$i", "$i<<$i", "$i>>>$i", "$i<<<$i", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"$i+$i", "$i-$i", //$NON-NLS-1$ //$NON-NLS-2$
				"$i*$i", "$i/$i", "$i%$i", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"$L as $t", //$NON-NLS-1$
				"$i**$i", //$NON-NLS-1$
				"!$R", "-$R", "+$R", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"$v++", "$v--"); //$NON-NLS-1$ //$NON-NLS-2$
		System.out.println(generateOperatorPrecedenceMarkdownTable(operators));
	}

	/** Generate a table that contains the description of the operator precedences.
	 *
	 * @param operators the list of operators. Each operator description should contain one of:<ul>
	 *     <li>{@code $v}, a variable;</li>
	 *     <li>{@code $i}, an integer value with unknown associativity;</li>
	 *     <li>{@code $L}, an integer value with left-to-right associativity;</li>
	 *     <li>{@code $R}, an integer value with right-to-left associativity;</li>
	 *     <li>{@code $o}, an object;</li>
	 *     <li>{@code $t}, a class.</li>
	 *     </ul>
	 * @return the Markdown table.
	 * @throws Exception if the code cannot be parsed.
	 */
	public static String generateOperatorPrecedenceMarkdownTable(List<String> operators) throws Exception {
		final var associativities = new HashMap<String, Associativity>();
		final var labels = new HashMap<String, String>();
		final var precedenceGroups = new ArrayList<List<String>>();
		buildOperatorInfo(operators, associativities, labels, precedenceGroups);
		
		// Build the table
		var length1 = 0;
		var length2 = 0;
		final var groups = new ArrayList<String>();
		final var assocs = new ArrayList<Associativity>();
		for (final var ops : precedenceGroups) {
			var first = true;
			final var result = new StringBuilder();
			Associativity assoc = null;
			for (final var op : ops) {
				if (first) {
					first = false;
					assoc = associativities.get(op);
				} else {
					result.append(", "); //$NON-NLS-1$
				}
				result.append(labels.get(op));
			}
			groups.add(result.toString());
			assert assoc != null;
			assocs.add(assoc);
			var len = result.length();
			if (len > length1) {
				length1 = len;
			}
			len = assoc.toLabel().length();
			if (len > length2) {
				length2 = len;
			}
		}
		final var result = new StringBuilder();
		result.append("| ").append(Messages.OperatorExtensions_0); //$NON-NLS-1$
		append(result, ' ', length1 - 9);
		result.append(" | ").append(Messages.OperatorExtensions_1); //$NON-NLS-1$
		append(result, ' ', length2 - 13);
		result.append(" |\n"); //$NON-NLS-1$
		result.append("| "); //$NON-NLS-1$				
		append(result, '-', length1);
		result.append(" | "); //$NON-NLS-1$
		append(result, '-', length2);
		result.append(" |\n"); //$NON-NLS-1$
		for (var i = 0; i < groups.size(); ++i) {
			final var ops = groups.get(i);
			final var assoc = assocs.get(i);
			result.append("| "); //$NON-NLS-1$
			result.append(ops);
			append(result, ' ', length1 - ops.length());
			result.append(" | "); //$NON-NLS-1$
			result.append(assoc.toLabel());
			append(result, ' ', length2 - assoc.toLabel().length());
			result.append(" |\n"); //$NON-NLS-1$
		}
		return result.toString();
	}

	/** Validate the order of the operators according to the operator precedence.
	 *
	 * @param operators the list of operators. Each operator description should contain one of:<ul>
	 *     <li>{@code $v}, a variable;</li>
	 *     <li>{@code $i}, an integer value with unknown associativity;</li>
	 *     <li>{@code $L}, an integer value with left-to-right associativity;</li>
	 *     <li>{@code $R}, an integer value with right-to-left associativity;</li>
	 *     <li>{@code $o}, an object;</li>
	 *     <li>{@code $t}, a class.</li>
	 *     </ul>
	 * @return validation status.
	 */
	public static boolean validateOperatorOrder(List<List<String>> operators) {
		final var logger = getInjector().getInstance(java.util.logging.Logger.class);
		final var result = validateOperatorOrder(operators, (message, exception) -> {
			if (exception != null) {
				logger.log(java.util.logging.Level.SEVERE, message, exception);
			} else {
				logger.severe(message);
			}
		});
		return result == 0;
	}

	/** Validate the order of the operators according to the operator precedence.
	 *
	 * @param operators the list of operators. Each operator description should contain one of:<ul>
	 *     <li>{@code $v}, a variable;</li>
	 *     <li>{@code $i}, an integer value with unknown associativity;</li>
	 *     <li>{@code $L}, an integer value with left-to-right associativity;</li>
	 *     <li>{@code $R}, an integer value with right-to-left associativity;</li>
	 *     <li>{@code $o}, an object;</li>
	 *     <li>{@code $t}, a class.</li>
	 *     </ul>
	 * @param errorLogger the logger of the error if there is any.
	 * @return the number of errors.
	 * @since 0.15
	 */
	public static int validateOperatorOrder(List<List<String>> operators, Procedure2<String, Throwable> errorLogger) {
		final var flatOperators = new ArrayList<String>();
		
		for (final var ops : operators) {
			flatOperators.addAll(ops);
		}

		final var associativities = new HashMap<String, Associativity>();
		final var labels = new HashMap<String, String>();
		final var precedenceGroups = new ArrayList<List<String>>();
		try {
			buildOperatorInfo(flatOperators, associativities, labels, precedenceGroups);
		} catch (Exception exception) {
			if (errorLogger != null) {
				errorLogger.apply(exception.getLocalizedMessage(), exception);
			}
			return 1;
		}

		var result = 0;
		final var iterator = operators.iterator();
		for (final var actualGroup : precedenceGroups) {
			if (!iterator.hasNext()) {
				error(errorLogger, Messages.OperatorExtensions_2, operators, precedenceGroups);
				++result;
			}
			final var originalExpectedGroup = iterator.next();
			final var expectedGroup = new ArrayList<>(originalExpectedGroup);
			if (expectedGroup.size() != actualGroup.size()) {
				error(errorLogger, MessageFormat.format(Messages.OperatorExtensions_3, actualGroup, originalExpectedGroup), operators, precedenceGroups);
				++result;
			}
			for (final var actualOp : actualGroup) {
				if (!expectedGroup.remove(actualOp)) {
					error(errorLogger, MessageFormat.format(Messages.OperatorExtensions_3, actualGroup, originalExpectedGroup), operators, precedenceGroups);
					++result;
				}
			}
			if (!expectedGroup.isEmpty()) {
				error(errorLogger, MessageFormat.format(Messages.OperatorExtensions_3, actualGroup, originalExpectedGroup), operators, precedenceGroups);
				++result;
			}
		}
		return result;
	}

	private static void error(Procedure2<String, Throwable> errorLogger, String message, List<List<String>> expectedPrecedence,
			List<List<String>> actualPrecedence) {
		if (errorLogger != null) {
			errorLogger.apply(message, null);
			errorLogger.apply(MessageFormat.format(Messages.OperatorExtensions_4, expectedPrecedence), null);
			errorLogger.apply(MessageFormat.format(Messages.OperatorExtensions_5, actualPrecedence), null);
		}
	}

	private static Injector getInjector() {
		return DocumentationSetup.doSetup();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static ParseHelper<SarlScript> getParseHelper() {
		if (parser == null) {
			parser = (Provider) getInjector().getProvider(ParseHelper.class);
			loggerOff(ProcessorInstanceForJvmTypeProvider.class, "logger"); //$NON-NLS-1$
		}
		return parser.get();
	}

	private static void loggerOff(Class<?> type, String name) {
		try {
			final var field = type.getDeclaredField(name);
			field.setAccessible(true);
			final var logger = (Logger) field.get(null);
			if (logger != null) {
				logger.setLevel(Level.OFF);
			}
		} catch (Exception exception) {
			throw new RuntimeException(exception);
		}
	}

	private static ValidationTestHelper getValidation() {
		if (validation == null) {
			validation = getInjector().getInstance(ValidationTestHelper.class);
		}
		return validation;
	}

	/** Parse the given SARL code and replies the EMF tree.
	 *
	 * @param code the SARL code to parse and convert to EMF.
	 * @return the EMF tree.
	 * @throws Exception if the code cannot be parsed.
	 */
	private static SarlScript scriptToEMF(String code) throws Exception {
		final var parser = getParseHelper();
		return parser.parse(code);
	}

	private static String getDefinitionsForOperators() {
		final String[] binaryOperators = {
				"or", "and", "equals", "notEquals", "tripleEquals", "tripleNotEquals", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
				"greaterThan", "lessThan", "greaterEqualsThan", "lessEqualsThan", "spaceship", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
				"diamond", "upTo", "greaterThanDoubleDot", "doubleDotLessThan", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"mappedTo", "doubleArrow", "elvis", "doubleGreaterThan", "doubleLessThan", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
				"tripleGreaterThan", "tripleLessThan", "plus", "minus", "multiply", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
				"divide", "modulo", "power" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		};
		final var buf = new StringBuilder();
		for (final var op : binaryOperators) {
			buf.append("static def operator_").append(op).append("(a : int, b : int) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
			buf.append("static def operator_").append(op).append("(a : boolean, b : int) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
			buf.append("static def operator_").append(op).append("(a : int, b : boolean) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
			buf.append("static def operator_").append(op).append("(a : boolean, b : boolean) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		final String[] unaryOperators = {
				"minus", "not", "plusPlus", "moinsMoins" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		};
		for (final var op : unaryOperators) {
			buf.append("static def operator_").append(op).append("(a : int) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
			buf.append("static def operator_").append(op).append("(a : boolean) : int {0}\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return buf.toString();
	}

	/** Parse the given SARL expression and replies the EMF tree.
	 *
	 * @param expression the SARL code to parse and convert to EMF.
	 * @param showError indicates if the error should be displayed.
	 * @return the EMF tree.
	 * @throws Exception if the code cannot be parsed.
	 */
	private static XExpression toEMFExpression(String expression, boolean showError) throws Exception {
		final var code = "class ___X {\n" //$NON-NLS-1$
				+ "static var OBJ = new Integer(1);\n" //$NON-NLS-1$
				+ "static var VAR = 1;\n" //$NON-NLS-1$
				+ getDefinitionsForOperators()
				+ "var __expr  : int = (" //$NON-NLS-1$
				+ expression
				+ ");\n}"; //$NON-NLS-1$
		final var script = scriptToEMF(code);
		try {
			getValidation().assertNoErrors(script.eResource());
		} catch (Throwable exception) {
			if (showError) {
				System.err.println(exception.getLocalizedMessage());
				System.err.println(code);
			}
			return null;
		}
		final var members = script.getXtendTypes().get(0).getMembers();
		final var field = (SarlField) members.get(members.size() - 1);
		return field.getInitialValue();
	}

	private static void dump(XExpression expr, OutParameter<String> raw, OutParameter<Integer> depth,
			OutParameter<Associativity> assoc) {
		if (expr instanceof XBinaryOperation operator) {
			final var name = QualifiedName.create(operator.getFeature().getSimpleName());
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			final var leftRaw = new OutParameter<String>();
			final var leftDepth = new OutParameter<Integer>();
			dump(operator.getLeftOperand(), leftRaw, leftDepth, null);
			rawb.append(leftRaw.get());
			rawb.append(getInjector().getInstance(OperatorMapping.class).getOperator(name));
			final var rightRaw = new OutParameter<String>();
			final var rightDepth = new OutParameter<Integer>();
			dump(operator.getRightOperand(), rightRaw, rightDepth, null);
			rawb.append(rightRaw.get());
			rawb.append(")"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(Math.max(leftDepth.get().intValue(), rightDepth.get().intValue()) + 1));
			}
			if (assoc != null) {
				if (leftDepth.get().intValue() > rightDepth.get().intValue()) {
					assoc.set(Associativity.LEFT_TO_RIGHT);
				} else if (leftDepth.get().intValue() < rightDepth.get().intValue()) {
					assoc.set(Associativity.RIGHT_TO_LEFT);
				} else {
					assoc.set(Associativity.NONE);
				}
			}
		} else if (expr instanceof XUnaryOperation operator) {
			final var name = QualifiedName.create(operator.getFeature().getSimpleName());
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			rawb.append(getInjector().getInstance(OperatorMapping.class).getOperator(name));
			final var leftRaw = new OutParameter<String>();
			final var leftDepth = new OutParameter<Integer>();
			dump(operator.getOperand(), leftRaw, leftDepth, null);
			rawb.append(leftRaw.get());
			rawb.append(")"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(leftDepth.get().intValue() + 1));
			}
			if (assoc != null) {
				if (leftDepth.get().intValue() > 1) {
					assoc.set(Associativity.RIGHT_TO_LEFT);
				} else {
					assoc.set(Associativity.NONE);
				}
			}
		} else if (expr instanceof XPostfixOperation operator) {
			final var name = QualifiedName.create(operator.getFeature().getSimpleName());
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			final var leftRaw = new OutParameter<String>();
			final var leftDepth = new OutParameter<Integer>();
			dump(operator.getOperand(), leftRaw, leftDepth, null);
			rawb.append(leftRaw.get());
			rawb.append(getInjector().getInstance(OperatorMapping.class).getOperator(name));
			rawb.append(")"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(leftDepth.get().intValue() + 1));
			}
			if (assoc != null) {
				if (leftDepth.get().intValue() > 1) {
					assoc.set(Associativity.LEFT_TO_RIGHT);
				} else {
					assoc.set(Associativity.NONE);
				}
			}
		} else if (expr instanceof XInstanceOfExpression instanceofExpr) {
			final var leftRaw = new OutParameter<String>();
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			final var leftDepth = new OutParameter<Integer>();
			dump(instanceofExpr.getExpression(), leftRaw, leftDepth, null);
			rawb.append(leftRaw.get());
			rawb.append(" instanceof T)"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(leftDepth.get().intValue() + 1));
			}
			if (assoc != null) {
				assoc.set(Associativity.NONE);
			}
		} else if (expr instanceof XCastedExpression castExpr) {
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			final var leftRaw = new OutParameter<String>();
			final var leftDepth = new OutParameter<Integer>();
			dump(castExpr.getTarget(), leftRaw, leftDepth, null);
			rawb.append(leftRaw.get());
			rawb.append(" as T)"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(leftDepth.get().intValue() + 1));
			}
			if (assoc != null) {
				if (leftDepth.get().intValue() > 1) {
					assoc.set(Associativity.LEFT_TO_RIGHT);
				} else {
					assoc.set(Associativity.NONE);
				}
			}
		} else if (expr instanceof XAssignment ass) {
			final var rawb = new StringBuilder();
			rawb.append("("); //$NON-NLS-1$
			final var leftRaw = new OutParameter<String>();
			final var leftDepth = new OutParameter<Integer>();
			if (ass.getActualReceiver() == null) {
				leftRaw.set(ass.getFeature().getSimpleName());
				leftDepth.set(Integer.valueOf(1));
			} else {
				dump(ass.getActualReceiver(), leftRaw, leftDepth, null);
			}
			rawb.append(leftRaw.get());
			rawb.append("="); //$NON-NLS-1$
			final var rightRaw = new OutParameter<String>();
			final var rightDepth = new OutParameter<Integer>();
			dump(ass.getValue(), rightRaw, rightDepth, null);
			rawb.append(rightRaw.get());
			rawb.append(")"); //$NON-NLS-1$
			if (raw != null) {
				raw.set(rawb.toString());
			}
			if (depth != null) {
				depth.set(Integer.valueOf(Math.max(leftDepth.get().intValue(), rightDepth.get().intValue()) + 1));
			}
			if (assoc != null) {
				if (leftDepth.get().intValue() > rightDepth.get().intValue()) {
					assoc.set(Associativity.LEFT_TO_RIGHT);
				} else if (leftDepth.get().intValue() < rightDepth.get().intValue()) {
					assoc.set(Associativity.RIGHT_TO_LEFT);
				} else {
					assoc.set(Associativity.NONE);
				}
			}
		} else if ((expr instanceof XBooleanLiteral) || expr instanceof XNumberLiteral || ((expr instanceof XFeatureCall))) {
			if (raw != null) {
				raw.set("x"); //$NON-NLS-1$
			}
			if (depth != null) {
				depth.set(Integer.valueOf(1));
			}
			if (assoc != null) {
				assoc.set(Associativity.NONE);
			}
		} else {
			throw new UnsupportedOperationException(Utils.dump(expr));
		}
	}

	private static void buildOperatorInfo(List<String> operators,
			Map<String, Associativity> associativities,
			Map<String, String> labels,
			List<List<String>> precedenceGroups) throws Exception {
		// Detect associativity of operator family
		final var operatorNames = new HashMap<String, List<String>>();
		for (final var operator : operators) {
			final var assoc = detectAssociativity(operator);
			assert assoc != null : "No associativity for " + operator; //$NON-NLS-1$
			associativities.put(operator, assoc);
			final var simpleText = toText(operator);
			var ops = operatorNames.get(simpleText);
			if (ops == null) {
				ops = new ArrayList<>();
				operatorNames.put(simpleText, ops);
			}
			ops.add(operator);
		}

		// Compute labels
		for (final var entry : operatorNames.entrySet()) {
			final var ops = entry.getValue();
			if (ops.size() > 1) {
				for (final var op : ops) {
					if (isBinaryOperator(op)) {
						labels.put(op, entry.getKey());
					} else {
						labels.put(op, entry.getKey() + " (unary)"); //$NON-NLS-1$
					}
				}
			} else {
				labels.put(ops.get(0), entry.getKey());
			}
		}

		// Compute precedences
		final var precBuffer = new HashMap<String, Integer>();
		final var sortedOperators = new ArrayList<>(operators);
		final Comparator<String> comp = (op1, op2) -> {
			final var key1 = op1 + "   " + op2; //$NON-NLS-1$
			var cmp = precBuffer.get(key1);
			if (cmp != null) {
				return cmp.intValue();
			}
			final var key2 = op2 + "   " + op1; //$NON-NLS-1$
			cmp = precBuffer.get(key2);
			if (cmp != null) {
				return -cmp.intValue();
			}
			if (!op1.contains("$i") && !op1.contains("$R") && !op1.contains("$L") && !op1.contains("$o")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				if (!op2.contains("$i") && !op2.contains("$R") && !op2.contains("$L") && !op2.contains("$o")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					cmp = Integer.valueOf(associativities.get(op1).compareTo(associativities.get(op2)));
					if (cmp.intValue() == 0) {
						if (!op1.contains("$v")) { //$NON-NLS-1$
							cmp = Integer.valueOf(1);
						} else if (!op2.contains("$v")) { //$NON-NLS-1$
							cmp = Integer.valueOf(-1);
						} else {
							cmp = Integer.valueOf(0);
						}
					}
					precBuffer.put(key1, cmp);
					precBuffer.put(key2, cmp);
					return cmp.intValue();
				}
				precBuffer.put(key1, Integer.valueOf(1));
				precBuffer.put(key2, Integer.valueOf(-1));
				return 1;
			}
			if (!op2.contains("$i") && !op2.contains("$R") && !op2.contains("$L") && !op2.contains("$o")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				precBuffer.put(key1, Integer.valueOf(-1));
				precBuffer.put(key2, Integer.valueOf(1));
				return -1;
			}
			final var binary1 = isBinaryOperator(op1);
			final String expr1;
			final String expr2;
			if (binary1) {
				final var rep = toSARLExpression(op2);
				expr1 = toSARLExpressionRight(op1, rep);
				expr2 = toSARLExpressionLeft(op1, rep);
			} else {
				final var binary2 = isBinaryOperator(op2);
				if (binary2) {
					final var rep = toSARLExpression(op1);
					expr1 = toSARLExpressionLeft(op2, rep);
					expr2 = toSARLExpressionRight(op2, rep);
				} else {
					final var rep2 = toSARLExpression(op2);
					final var rep1 = toSARLExpression(op1);
					expr1 = toSARLExpressionRight(op1, rep2);
					expr2 = toSARLExpressionRight(op2, rep1);
				}
			}
			final var prec1 = computePrecedence(expr1);
			final var prec2 = computePrecedence(expr2);
			if (prec1 == null && prec2 == null) {
				cmp = Integer.valueOf(associativities.get(op1).compareTo(associativities.get(op2)));
				precBuffer.put(key1, cmp);
				precBuffer.put(key2, Integer.valueOf(-cmp.intValue()));
				return cmp.intValue();
			}
			if (prec1 == null && prec2 != null) {
				precBuffer.put(key1, Integer.valueOf(-prec2.intValue()));
				precBuffer.put(key2, Integer.valueOf(prec2.intValue()));
				return -prec2.intValue();
			}
			if (prec1 != null && prec2 == null) {
				precBuffer.put(key1, Integer.valueOf(prec1.intValue()));
				precBuffer.put(key2, Integer.valueOf(-prec1.intValue()));
				return prec1.intValue();
			}
			assert prec1 != null && prec2 != null;
			if (prec1.intValue() == prec2.intValue()) {
				cmp = Integer.valueOf(associativities.get(op1).compareTo(associativities.get(op2)));
				precBuffer.put(key1, cmp);
				precBuffer.put(key2, Integer.valueOf(-cmp.intValue()));
				return cmp.intValue();
			}
			// Not same precedence
			precBuffer.put(key1, Integer.valueOf(prec1.intValue()));
			precBuffer.put(key2, Integer.valueOf(-prec1.intValue()));
			return prec1.intValue();
		};
		sortedOperators.sort(comp);

		final var iterator = sortedOperators.iterator();
		var op1 = iterator.next();
		var group = new ArrayList<String>();
		precedenceGroups.add(group);
		group.add(op1);
		while (iterator.hasNext()) {
			final var op2 = iterator.next();
			final var prec = comp.compare(op1, op2);
			if (prec != 0) {
				group = new ArrayList<>();
				precedenceGroups.add(group);
			}
			group.add(op2);
			op1 = op2;
		}
	}

	private static Integer computePrecedence(String expr) {
		try {
			final var xexpr = toEMFExpression(expr, SHOW_ERROR_PRECEDENCE);
			if (xexpr != null) {
				final var raw = new OutParameter<String>();
				final var depth = new OutParameter<Integer>();
				final var assoc = new OutParameter<Associativity>();
				dump(xexpr, raw, depth, assoc);
				switch(assoc.get()) {
				case LEFT_TO_RIGHT:
					return Integer.valueOf(1);
				case RIGHT_TO_LEFT:
					return Integer.valueOf(-1);
				case NONE:
					return Integer.valueOf(0);
				default:
				}
			}
		} catch (Exception exception) {
			throw new RuntimeException(exception);
		}
		return null;
	}

	private static void append(StringBuilder result, char c, int len) {
		for (var i = 0; i < len; ++i) {
			result.append(c);
		}
	}
	private static Associativity detectAssociativity(String operator) throws Exception {
		if (operator.contains("$L")) { //$NON-NLS-1$
			return Associativity.LEFT_TO_RIGHT;
		}
		if (operator.contains("$R")) { //$NON-NLS-1$
			return Associativity.RIGHT_TO_LEFT;
		}
		final var binaryOperator = isBinaryOperator(operator);
		if (binaryOperator
				|| operator.contains("$i") //$NON-NLS-1$
				|| operator.contains("$v")) { //$NON-NLS-1$
			final var expr = toSARLExpressionRight(operator, toSARLExpressionRight(operator, toSARLExpression(operator)));
			final var xexpr = toEMFExpression(expr, SHOW_ERROR_ASSOCIATIVITY);
			if (xexpr != null) {
				final var raw = new OutParameter<String>();
				final var depth = new OutParameter<Integer>();
				final var assoc = new OutParameter<Associativity>();
				dump(xexpr, raw, depth, assoc);
				return assoc.get();
			}
		}
		return Associativity.NONE;
	}

	private static int indexOfOperand(String operator, int start) {
		var idx = operator.indexOf('$', start);
		while (idx >= 0) {
			if ((idx - 1) < operator.length()) {
				final var c = operator.charAt(idx + 1);
				if (c == 'i' || c =='R' || c == 'L' && c =='o') {
					break;
				}
				idx = operator.indexOf('$', idx + 1);
			} else {
				return -1;
			}
		}
		if (idx < 0) {
			return -1;
		}
		return idx;
	}
	
	private static boolean isBinaryOperator(String operator) {
		final var idx1 = indexOfOperand(operator, 0);
		if (idx1 >= 0) {
			final var idx2 = indexOfOperand(operator, idx1 + 1);
			if (idx2 > idx1) {
				return true;
			}
		}
		return false;
	}

	private static String toSARLExpression(String operator) {
		var result = operator.replaceAll("\\$i", "1"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$L", "1"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$R", "1"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$o", "OBJ"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$t", "java.lang.Integer"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$v", "VAR"); //$NON-NLS-1$ //$NON-NLS-2$
		return result;
	}

	private static String toSARLExpressionRight(String operator, String rightOperand) {
		var result = replaceRight(operator, "$i", "1", rightOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceRight(result, "$L", "1", rightOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceRight(result, "$R", "1", rightOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceRight(result, "$o", "OBJ", rightOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$t", "java.lang.Integer"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$v", "VAR"); //$NON-NLS-1$ //$NON-NLS-2$
		return result;
	}

	private static String toSARLExpressionLeft(String operator, String leftOperand) {
		var result = replaceLeft(operator, "$i", "1", leftOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceLeft(result, "$L", "1", leftOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceLeft(result, "$R", "1", leftOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = replaceLeft(result, "$o", "OBJ", leftOperand); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$t", "java.lang.Integer"); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$v", "VAR"); //$NON-NLS-1$ //$NON-NLS-2$
		return result;
	}

	private static String replaceRight(String str, String replaced, String replacement, String lastReplacement) {
		var index = str.lastIndexOf(replaced);
		if (index >= 0) {
			final var r = new StringBuilder();
			if (index + replaced.length() < str.length()) {
				r.append(str.substring(index + replaced.length()));
			}
			r.insert(0, lastReplacement);
			var endIndex = index; 
			index = str.lastIndexOf(replaced, endIndex - 1);
			while (index >= 0) {
				r.insert(0, str.substring(index + replaced.length(), endIndex));
				r.insert(0, replacement);
				endIndex = index; 
				index = str.lastIndexOf(replaced, endIndex - 1);
			}
			if (endIndex > 0) {
				r.insert(0, str.substring(0, endIndex));
			}
			return r.toString();
		}
		return str;
	}

	private static String replaceLeft(String str, String replaced, String replacement, String firstReplacement) {
		var index = str.indexOf(replaced);
		if (index >= 0) {
			final var r = new StringBuilder();
			if (index > 0) {
				r.append(str.substring(0, index));
			}
			r.append(firstReplacement);
			var prevIndex = index; 
			index = str.indexOf(replaced, prevIndex + 1);
			while (index >= 0) {
				r.append(str.substring(prevIndex + replaced.length(), index));
				r.append(replacement);
				prevIndex = index; 
				index = str.indexOf(replaced, prevIndex + 1);
			}
			if ((prevIndex + replaced.length()) < str.length()) {
				r.append(str.substring(prevIndex + replaced.length()));
			}
			return r.toString();
		}
		return str;
	}

	private static String toText(String operator) {
		var result = operator.replaceAll("\\$i", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$L", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$R", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$o", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$t", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.replaceAll("\\$v", ""); //$NON-NLS-1$ //$NON-NLS-2$
		result = result.trim();
		return result;
	}

	private enum Associativity {
		NONE {

			@Override
			public String toLabel() {
				return Messages.OperatorExtensions_6;
			}
			
		},
		LEFT_TO_RIGHT {
			@Override
			public String toLabel() {
				return Messages.OperatorExtensions_7;
			}
		},
		RIGHT_TO_LEFT {
			@Override
			public String toLabel() {
				return Messages.OperatorExtensions_8;
			}
		};
		
		/** Replies the human-reading description of the associativity.
		 *
		 * @return the text.
		 */
		public abstract String toLabel();

	}

}
