/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.atomicdouble;

import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toDoubleObject;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toInt;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toShort;

import com.google.common.util.concurrent.AtomicDouble;
import org.junit.Test;

import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public class CodeTest extends AbstractSarlTest {

	private static AtomicDouble left = new AtomicDouble(4);

	@Test
	public void toFloat_AtomicLong() {
		assertEpsilonEquals(4, toFloat(left));
	}

	@Test
	public void toByte_AtomicLong() {
		assertEquals(4, toByte(left));
	}

	@Test
	public void toLong_AtomicLong() {
		assertEquals(4, toLong(left));
	}

	@Test
	public void toDouble_AtomicLong() {
		assertEpsilonEquals(4, toDouble(left));
	}

	@Test
	public void toShort_AtomicLong() {
		assertEquals(4, toShort(left));
	}

	@Test
	public void toInt_AtomicLong() {
		assertEquals(4, toInt(left));
	}

	@Test
	public void toIntegerInteger_AtomicLong() {
		Double value = toDoubleObject(left);
		assertNotNull(value);
		assertEpsilonEquals(4, value.doubleValue());
	}

}
