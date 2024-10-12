/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.lang.tests.api.tools;

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;

import com.google.common.collect.Iterables;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;

import io.sarl.lang.sarl.SarlFormalParameter;

/** Set of utility classes that provide additional assertion functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class TestAssertions {

	private TestAssertions() {
		//
	}

	/** Assert that the given actual formal parameters have the expected default values.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * The parameter {@code expectedDefaultValues} is a sequence of pairs, where
	 * the first element is the type of the default value and the second element is
	 * the representation of the default value.
	 * If the first element of the pair is null (meaning no default value), then
	 * the second element must be missed.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedDefaultValues the expected default values.
	 */
	@SuppressWarnings("rawtypes")
	public static void assertParameterDefaultValues(Iterable<? extends XtendParameter> actualFormalParameters, Object... expectedDefaultValues) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			if (expectedDefaultValues[i] == null) {
				if (parameter instanceof SarlFormalParameter) {
					assertNull("No default value expected", ((SarlFormalParameter) parameter).getDefaultValue()); //$NON-NLS-1$
				}
			} else {
				assertTrue(parameter instanceof SarlFormalParameter);
				final int ii = i;
				assertTrue(expectedDefaultValues[i] instanceof Class, () -> "The #" + ii + " in expectedDefaultValues is not a Class"); //$NON-NLS-1$ //$NON-NLS-2$
				Class type = (Class) expectedDefaultValues[i];
				assertTrue(type.isInstance(((SarlFormalParameter) parameter).getDefaultValue()), () -> "Unexpected type for the default value."); //$NON-NLS-1$
				if (XNumberLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XNumberLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XStringLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XStringLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XNullLiteral.class.isAssignableFrom(type)) {
					//
				} else {
					throw new RuntimeException("Unsupported type of literal for this assertion function"); //$NON-NLS-1$
				}
			}
			++i;
		}
		if (i < expectedDefaultValues.length) {
			fail("Not enough default values. Expected: " + Arrays.toString(expectedDefaultValues) //$NON-NLS-1$
				+ "Actual: " + Iterables.toString(actualFormalParameters)); //$NON-NLS-1$
		}
	}

}
