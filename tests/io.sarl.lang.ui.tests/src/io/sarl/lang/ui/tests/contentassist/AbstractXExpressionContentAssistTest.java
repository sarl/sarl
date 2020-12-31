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

package io.sarl.lang.ui.tests.contentassist;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.core.resources.IFile;
import org.eclipse.xtext.AbstractElement;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.IGrammarAccess;
import org.eclipse.xtext.junit4.ui.ContentAssistProcessorTestBuilder;
import org.eclipse.xtext.junit4.util.ResourceLoadHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.junit.Test;

import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.WorkbenchTestHelper;

@SuppressWarnings("all")
public abstract class AbstractXExpressionContentAssistTest extends AbstractContentAssistTest {

//	@Test public void testOnStringLiteral_01() throws Exception {
//		newBuilder().append("''").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_02() throws Exception {
//		newBuilder().append("''.").assertText(getStringFeatures());
//	}
//	
//	@Test public void testOnStringLiteral_03() throws Exception {
//		newBuilder().append("''.").assertTextAtCursorPosition(".", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_04() throws Exception {
//		newBuilder().append("''+''").assertTextAtCursorPosition("+", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_05() throws Exception {
//		newBuilder().append("''+''").assertTextAtCursorPosition("+''", 1, expect(new String[]{"+"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_06() throws Exception {
//		newBuilder().append("''==''").assertTextAtCursorPosition("==", 1, "==", "=>", "===");
//	}
//	
//	@Test public void testOnStringLiteral_07() throws Exception {
//		newBuilder().append("''==''").assertTextAtCursorPosition("==", 2, expect(new String[]{"==", "==="}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_08() throws Exception {
//		newBuilder().append("''<=''").assertTextAtCursorPosition("<=", 1, expect(new String[]{"<", "<=", "<=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_09() throws Exception {
//		newBuilder().append("''<=''").assertTextAtCursorPosition("<=", 2, expect(new String[]{"<=", "<=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_10() throws Exception {
//		newBuilder().append("'' ").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_11() throws Exception {
//		newBuilder().append("''. ").assertText(getStringFeatures());
//	}
//	
//	@Test public void testOnStringLiteral_12() throws Exception {
//		newBuilder().append("'' .").assertTextAtCursorPosition(".", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_13() throws Exception {
//		newBuilder().append("'' + ''").assertTextAtCursorPosition("+", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_14() throws Exception {
//		newBuilder().append("'' + ''").assertTextAtCursorPosition("+ ''", 2, getKeywordsAndStatics());
//		newBuilder().append("'' + ''").assertTextAtCursorPosition("+ ''", 1, expect(new String[]{"+"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_15() throws Exception {
//		newBuilder().append("'' == ''").assertTextAtCursorPosition("==", 1, "==", "=>", "===");
//	}
//	
//	@Test public void testOnStringLiteral_16() throws Exception {
//		newBuilder().append("'' == ''").assertTextAtCursorPosition("==", 2, expect(new String[]{"==", "==="}, getKeywordsAndStatics()));
//		newBuilder().append("'' == ''").assertTextAtCursorPosition("==", 3, getKeywordsAndStatics());
//	}
//	
//	@Test public void testOnStringLiteral_17() throws Exception {
//		newBuilder().append("'' <= ''").assertTextAtCursorPosition("<=", 1, expect(new String[]{"<", "<=", "<=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_18() throws Exception {
//		newBuilder().append("'' <= ''").assertTextAtCursorPosition("<=", 2, expect(new String[]{"<=", "<=>"}, getKeywordsAndStatics()));
//		newBuilder().append("'' <= ''").assertTextAtCursorPosition("<=", 3, getKeywordsAndStatics());
//	}
//	
//	@Test public void testOnStringLiteral_19() throws Exception {
//		newBuilder().append("''.toString").assertText(expect(STRING_OPERATORS, new String[]{"toString"}));
//	}
//	
//	@Test public void testOnStringLiteral_20() throws Exception {
//		newBuilder().append("''.toString.").assertText(getStringFeatures());
//	}
//	
//	@Test public void testOnStringLiteral_21() throws Exception {
//		newBuilder().append("''.toString.").assertTextAtCursorPosition("g.", 1, expect(STRING_OPERATORS, new String[]{"toString"}));
//	}
//	
//	@Test public void testOnStringLiteral_22() throws Exception {
//		newBuilder().append("''.toString+''").assertTextAtCursorPosition("+", expect(STRING_OPERATORS, new String[]{"toString"}));
//	}
//	
//	@Test public void testOnStringLiteral_23() throws Exception {
//		newBuilder().append("''.toString+''").assertTextAtCursorPosition("+''", 1, expect(new String[]{"+"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_24() throws Exception {
//		newBuilder().append("''.toString==''").assertTextAtCursorPosition("==", 1, expect(new String[] {"===", "==", "=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_25() throws Exception {
//		newBuilder().append("''.toString==''").assertTextAtCursorPosition("==", 2, expect(new String[]{"==", "==="}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_26() throws Exception {
//		newBuilder().append("''.toString<=''").assertTextAtCursorPosition("<=", 1, expect(new String[]{"<", "<=", "<=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_27() throws Exception {
//		newBuilder().append("''.toString<=''").assertTextAtCursorPosition("<=", 2, expect(new String[]{"<=", "<=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_28() throws Exception {
//		newBuilder().append("''.toString.toString").assertTextAtCursorPosition(".", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_29() throws Exception {
//		newBuilder().append("''.toString.toString.toString").assertTextAtCursorPosition("g.", 1, expect(STRING_OPERATORS, new String[]{"toString"}));
//	}
//	
//	@Test public void testOnStringLiteral_30() throws Exception {
//		newBuilder().append("('')").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_31() throws Exception {
//		newBuilder().append("('').").assertText(getStringFeatures());
//	}
//	
//	@Test public void testOnStringLiteral_32() throws Exception {
//		newBuilder().append("(''.toString)").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_33() throws Exception {
//		newBuilder().append("(''.toString )").assertTextAtCursorPosition(")", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_34() throws Exception {
//		newBuilder().append("''.toString ").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//
//	@Test public void testOnStringLiteral_35() throws Exception {
//		newBuilder().append("''.toString .").assertTextAtCursorPosition("g .", 2, expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_36() throws Exception {
//		newBuilder().append("''.toString +''").assertTextAtCursorPosition("+", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testOnStringLiteral_37() throws Exception {
//		newBuilder().append("''.toString ==''").assertTextAtCursorPosition("==", 1, expect(new String[] {"==", "===", "=>"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testOnStringLiteral_38() throws Exception {
//		newBuilder().append("''.toString.toString").assertTextAtCursorPosition("g.", 1, expect(STRING_OPERATORS, new String[]{"toString"}));
//	}
//	
//	@Test public void testOnStringLiteral_39() throws Exception {
//		newBuilder().append("''.invalid = ''").assertTextAtCursorPosition(".", 1, getStringFeatures());
//	}
//	
//	@Test public void testOnStringLiteral_40() throws Exception {
//		newBuilder().append("''.length = 'invalid'").assertTextAtCursorPosition("le", 2, new String[]{"length"});
//	}
//
//	@Test public void testOnVoidMethod_01() throws Exception {
//		newBuilder().append("(null as java.util.List).clear ").assertText();
//	}
//	
//	@Test public void testAfterBinaryOperation_01() throws Exception {
//		newBuilder().append("''+''").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testAfterBinaryOperation_02() throws Exception {
//		newBuilder().append("'' + ''+''").assertTextAtCursorPosition("''+", 2, expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testAfterBinaryOperation_03() throws Exception {
//		newBuilder().append("(''+'')").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//
//	@Test public void testAfterBinaryOperation_04() throws Exception {
//		newBuilder().append("(''+'').").assertText(getStringFeatures());
//	}
//	
//	@Test public void testAfterBinaryOperation_05() throws Exception {
//		newBuilder().append("((''+''))").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testAfterBinaryOperation_06() throws Exception {
//		newBuilder().append("((''+''))").assertTextAtCursorPosition("))", 1, expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//	
//	@Test public void testAfterBinaryOperation_07() throws Exception {
//		newBuilder().append("((''+''))").assertTextAtCursorPosition("))", expect(STRING_OPERATORS, CAST_INSTANCEOF));
//	}
//
//	@Test public void testAfterBinaryOperation_08() throws Exception {
//		newBuilder().append("((''+(''.bytes)))").assertTextAtCursorPosition(")", "==", "===", "!=", "!==", "+=", "-=", "+", "->", "=>", "?:", "bytes");
//	}
//	
//	@Test public void testAfterBinaryOperation_09() throws Exception {
//		newBuilder().append("((''+''.bytes))").assertTextAtCursorPosition(")", "==", "===", "!=", "!==", "+=", "-=", "+", "->", "=>", "?:", "bytes");
//	}
//	
//	@Test public void testAfterBinaryOperation_10() throws Exception {
//		newBuilder().append("((''+null))").assertTextAtCursorPosition(")", 
//				"null", "!=", "!==", "==", "===", 
//				"->", "=>", 
//				"+", 
//				"?:",
//				"<", "<=", "<=>", ">=", ">");
//	}
//	
//	@Ignore("TODO binary operator precedence is not implemented in CA yet")
//	@Test public void testAfterBinaryOperation_11() throws Exception {
//		newBuilder().append("''+1").assertText(expect(STRING_OPERATORS, CAST_INSTANCEOF /* number operators */));
//	}
//	
//	@Test public void testStaticFeatures_01() throws Exception {
//		newBuilder().append("String::").assertText(getStaticStringFeatures());
//	}
//	
//	@Test public void testStaticFeatures_02() throws Exception {
//		newBuilder().append("String.").assertText(expect(getStaticStringFeatures(), getClassFeatures()));
//	}
//
//	@Test public void testNestedTypes_01() throws Exception {
//		newBuilder().append("java.util.Map.").assertText(expect(new String[] {"Entry"}, getClassFeatures()));
//	}
//	
//	@Test public void testNull() throws Exception {
//		newBuilder().append("null").assertText("null", "!=", "!==", "+", "==", "===", "->", "?:", "=>",
//				"%", "*", "**", "-", "+=", "-=", "/", "<", "<=", "<=>", ">=", ">", "++", "--");
//	}
//	
//	@Test public void testForLoop_01() throws Exception {
//		newBuilder().append("for (String s: null) ").assertText(expect(new String[]{"s"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testForLoop_02() throws Exception {
//		newBuilder().append("for (String string: null) string").assertTextAtCursorPosition(") string", 6, "string");
//	}
//	
//	@Test public void testForLoop_03() throws Exception {
//		newBuilder().append("for (String string: null) ''+").assertText(expect(new String[] {"string", "+"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testForLoop_04() throws Exception {
//		newBuilder().append("for (String string: ").assertText(getKeywordsAndStatics());
//	}
//	
//	@Test public void testForLoop_05() throws Exception {
//		newBuilder().append("for (String string: )").assertTextAtCursorPosition(")", getKeywordsAndStatics());
//	}
//	
//	@Test public void testClosure_01() throws Exception {
//		newBuilder().append("[String a, String b|").assertText(expect(new String[]{"a", "b"}, getKeywordsAndStatics(), new String[] {"val", "var"}));
//	}
//	
//	@Test public void testClosure_02() throws Exception {
//		newBuilder().append("#['a', 'b'].filter[it==it]").assertTextAtCursorPosition("it==", expect(new String[]{"it", "var", "val", "self"}, getKeywordsAndStatics(), getStringFeatures()));
//	}
//
//	@Test public void testClosure_03() throws Exception {
//		newBuilder().append("{val slow = #['a', 'b'].filter[it==it] }").assertTextAtCursorPosition("it==", expect(new String[]{"it", "var", "val", "self"}, getKeywordsAndStatics(), getStringFeatures()));
//	}
//	
//	@Test public void testClosure_04() throws Exception {
//		newBuilder().append("{val slow = #[0bd, 1bd].filter[i > 0]}").assertTextAtCursorPosition("i ", expect(new String[]{"it", "var", "val", "self"}, getKeywordsAndStatics(), getBigDecimalFeatures()));
//	}
//	
//	@Test public void testCatchParameter_01() throws Exception {
//		newBuilder().append("try {} catch(NullPointerException e) e").assertTextAtCursorPosition(") e", 2, expect(new String[]{"e"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testCatchParameter_02() throws Exception {
//		newBuilder().append("try {} catch(NullPointerException e) ").assertText(expect(new String[]{"e"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testCamelCase_01() throws Exception {
//		newBuilder().append("newLLis").assertText("newLinkedList()");
//	}
//	
//	@Test public void testCamelCase_02() throws Exception {
//		newBuilder().append("new ArrBloQu").assertText("java.util.concurrent.ArrayBlockingQueue");
//	}
//	
//	@Test public void testCamelCase_03() throws Exception {
//		newBuilder().append("new ArrBloQu").assertText("java.util.concurrent.ArrayBlockingQueue");
//	}
//	
//	@Test public void testSwitchOnEnum_01() throws Exception {
//		newBuilder().append("switch java.lang.annotation.RetentionPolicy.SOURCE { case ").assertText(expect(
//				new String[]{"SOURCE", "CLASS", "RUNTIME"}, getKeywordsAndStatics()));
//	}
//	
//	@Test public void testSwitchOnEnum_02() throws Exception {
//		newBuilder().append("switch java.lang.annotation.RetentionPolicy.SOURCE { case SOUR").assertProposal("SOURCE");
//	}
//	
//	@Test public void testSwitchOnEnum_03() throws Exception {
//		newBuilder().append("switch java.lang.annotation.RetentionPolicy.SOURCE { case SOURCE: ").assertText(getKeywordsAndStatics());
//	}

}
