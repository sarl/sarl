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

package io.sarl.lang.scoping.extensions.numbers.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {@code BigInteger}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public final class BigIntegerCastExtensions {

	private BigIntegerCastExtensions() {
		//
	}

	/** Convert the given value to {@code AtomicInteger}. This function is not null-safe.
	 *
	 * @param number a number of {@code BigInteger} type.
	 * @return the equivalent value to {@code number} of {@code AtomicInteger} type.
	 */
	@Pure
	@Inline(value = "new $2($1.intValue())", imported = AtomicInteger.class)
	public static AtomicInteger toAtomicInteger(BigInteger number) {
		return new AtomicInteger(number.intValue());
	}

	/** Convert the given value to {@code AtomicLong}. This function is not null-safe.
	 *
	 * @param number a number of {@code BigInteger} type.
	 * @return the equivalent value to {@code number} of {@code AtomicLong} type.
	 */
	@Pure
	@Inline(value = "new $2($1.longValue())", imported = AtomicLong.class)
	public static AtomicLong toAtomicLong(BigInteger number) {
		return new AtomicLong(number.longValue());
	}

	/** Convert the given value to {@code AtomicDouble}. This function is not null-safe.
	 *
	 * @param number a number of {@code BigInteger} type.
	 * @return the equivalent value to {@code number} of {@code AtomicDouble} type.
	 */
	@Pure
	@Inline(value = "new $2($1.doubleValue())", imported = AtomicDouble.class)
	public static AtomicDouble toAtomicDouble(BigInteger number) {
		return new AtomicDouble(number.doubleValue());
	}

	/** Convert the given value to {@code BigDecimal}.
	 *
	 * @param number a number of {@code BigInteger} type.
	 * @return the equivalent value to {@code number} of {@code BigDecimal} type.
	 */
	@Pure
	@Inline(value = "new $2($1)", imported = BigDecimal.class)
	public static BigDecimal toBigDecimal(BigInteger number) {
		return new BigDecimal(number);
	}

}
