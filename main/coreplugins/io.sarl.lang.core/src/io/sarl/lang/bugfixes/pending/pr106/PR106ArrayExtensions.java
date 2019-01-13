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

package io.sarl.lang.bugfixes.pending.pr106;

import com.google.common.annotations.GwtCompatible;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Extend the ArrayExtensions class for accessing multi-dimensional arrays.
 *
 * <p>FIXME: Remove when PR is merged: https://github.com/eclipse/xtext-lib/pull/106
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-lib/pull/106"
 */
@SuppressWarnings("checkstyle:all")
@GwtCompatible
public class PR106ArrayExtensions {

	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static <T, E extends T> T set(T[][] array, int index0, int index1, E value) {
		array[index0][index1] = value;
		return value;
	}

	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given index
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static <T> T get(T[][] array, int index0, int index1) {
		return array[index0][index1];
	}

	

	// BEGIN generated code
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static boolean get(boolean[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static boolean set(boolean[][] array, int index0, int index1, boolean value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static double get(double[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexex
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static double set(double[][] array, int index0, int index1, double value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the first index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static float get(float[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static float set(float[][] array, int index0, int index1, float value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static long get(long[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static long set(long[][] array, int index0, int index1, long value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2]")
	public static int get(int[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given index
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static int set(int[][] array, int index0, int index1, int value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static char get(char[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static char set(char[][] array, int index0, int index1, char value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static short get(short[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static short set(short[][] array, int index0, int index1, short value) {
		array[index0][index1] = value;
		return value;
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index
	 * @param index1
	 *            the second index
	 * @return the value at the given indexes
	 * @since 2.15
	 */
	@Pure
	@Inline("$1[$2][$3]")
	public static byte get(byte[][] array, int index0, int index1) {
		return array[index0][index1];
	}
	
	/**
	 * @param array
	 *            the array
	 * @param index0
	 *            the first index the value should be set at
	 * @param index1
	 *            the second index the value should be set at
	 * @param value
	 *            the value to set at the given indexes
	 * @return the new value
	 * @since 2.15
	 */
	@Inline("$1[$2][$3] = $4")
	public static byte set(byte[][] array, int index0, int index1, byte value) {
		array[index0][index1] = value;
		return value;
	}

}
