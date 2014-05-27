/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.util;

import java.util.BitSet;

/**
 * Utilities functions on BitSet
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BitSetUtil {

	/** Build a bitset from a long value.
	 * 
	 * @param value
	 * @return the bit set
	 */
	public static BitSet convert(long value) {
		long v = value;
		BitSet bits = new BitSet();
		int index = 0;
		while (v != 0L) {
			if (v % 2L != 0) {
				bits.set(index);
			}
			++index;
			v = v >>> 1;
		}
		return bits;
	}

	/** Create a long representation of a bitset.
	 * 
	 * @param bits
	 * @return the long representation.
	 */
	public static long convert(BitSet bits) {
		long value = 0L;
		for (int i = 0; i < bits.length(); ++i) {
			value += bits.get(i) ? (1L << i) : 0L;
		}
		return value;
	}

}
