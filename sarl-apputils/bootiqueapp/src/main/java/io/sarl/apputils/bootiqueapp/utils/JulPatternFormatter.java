/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.apputils.bootiqueapp.utils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import com.google.common.base.Strings;

/** JUL formatter based on pattern.
 *
 * <p>The format string may contains one of the following elements:<ul>
 * <li>{@code %1}: the date,</li>
 * <li>{@code %2}: the name of the calling function,</li>
 * <li>{@code %3}: the name of the logger,</li>
 * <li>{@code %4}: the logging level,</li>
 * <li>{@code %5}: the message, and</li>
 * <li>{@code %6}: the throwable.</li>
 * </ul>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class JulPatternFormatter extends Formatter {

	private final String pattern;

	private Date dat = new Date();

	/** Constructor.
	 *
	 * @param pattern the pattern that is compatible with {@link String#format(String, Object...)}.
	 */
	public JulPatternFormatter(String pattern) {
		assert !Strings.isNullOrEmpty(pattern);
		this.pattern = pattern;
	}

	@Override
	public String format(LogRecord record) {
		this.dat.setTime(record.getMillis());
		final StringBuilder source = new StringBuilder();
		final String scn = record.getSourceClassName();
		final String logName = record.getLoggerName();
		if (!Strings.isNullOrEmpty(scn)) {
			source.append(scn);
			final String smn = record.getSourceMethodName();
			if (!Strings.isNullOrEmpty(smn)) {
				source.append(" "); //$NON-NLS-1$
				source.append(smn);
			}
		} else {
			source.append(logName);
		}
		final String message = formatMessage(record);
		String throwable;
		if (record.getThrown() != null) {
			final StringWriter sw = new StringWriter();
			try (PrintWriter pw = new PrintWriter(sw)) {
				pw.println();
				record.getThrown().printStackTrace(pw);
			}
			throwable = sw.toString();
		} else {
			throwable = ""; //$NON-NLS-1$
		}
		return String.format(
				this.pattern,
				this.dat, source,
				filterLogName(logName),
				record.getLevel().getLocalizedName(),
				message,
				throwable);
	}

	/** Filter the log name in order to computer the one that is displayed into the log.
	 *
	 * @param logName the log name to filter.
	 * @return the displayable log name.
	 */
	@SuppressWarnings("static-method")
	protected String filterLogName(String logName) {
		return logName;
	}

}
