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

package io.sarl.lang.sarlc.configs;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * Style of the progress bar.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public enum ProgressBarStyle {
	/** Pure ASCII style.
	 */
	ASCII {

		@Override
		public me.tongfei.progressbar.ProgressBarStyle toBackgroundStyle() {
			return me.tongfei.progressbar.ProgressBarStyle.ASCII;
		}

	},

	/** Style with Unicode characters.
	 */
	UNICODE {

		@Override
		public me.tongfei.progressbar.ProgressBarStyle toBackgroundStyle() {
			return me.tongfei.progressbar.ProgressBarStyle.UNICODE_BLOCK;
		}

	},

	/** Style with Unicode characters and colors.
	 */
	COLORED_UNICODE {

		@Override
		public me.tongfei.progressbar.ProgressBarStyle toBackgroundStyle() {
			return me.tongfei.progressbar.ProgressBarStyle.COLORFUL_UNICODE_BLOCK;
		}

	};

	/** Replies the style related to the background API.
	 *
	 * @return the background API's of the style.
	 */
	public abstract me.tongfei.progressbar.ProgressBarStyle toBackgroundStyle();

	/** Parse the given case insensitive string for obtaining the progress bar style.
	 *
	 * @param name the string to parse.
	 * @return the progress bar style.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	public static ProgressBarStyle valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("Name is null"); //$NON-NLS-1$
		}
		return valueOf(name.toUpperCase());
	}

	/** Replies the Json string representation of this progress bar style.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

}
