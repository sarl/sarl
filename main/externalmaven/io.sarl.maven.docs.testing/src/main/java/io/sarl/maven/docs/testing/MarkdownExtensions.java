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

package io.sarl.maven.docs.testing;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

import io.sarl.lang.util.CliUtilities;

/** Tools for generating markdown from different sources.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public final class MarkdownExtensions {

	private MarkdownExtensions() {
		//
	}

	/** Render the option help to a Markdown table.
	 *
	 * @param options the options.
	 * @return the markdown table.
	 */
	@SuppressWarnings("unchecked")
	public static String renderToMarkdown(Object options) {
		if (options instanceof Options) {
			return _renderToMarkdown((Options) options);
		}
		if (options instanceof List<?>) {
			final List<?> rootList = (List<?>) options;
			if (!rootList.isEmpty()) {
				Object element = rootList.get(0);
				if (element instanceof List<?>) {
					return _renderToMarkdown((List<List<String>>) options);
				}
			} else {
				return _renderToMarkdown((List<List<String>>) options);
			}
		}
		return ""; //$NON-NLS-1$
	}

	/** Render the option help to a Markdown table.
	 *
	 * @param options the options.
	 * @return the markdown table.
	 */
	protected static String _renderToMarkdown(Options options) {
		if (options == null) {
			return ""; //$NON-NLS-1$
		}
		final List<Option> optList = new ArrayList<>(options.getOptions());
		if (optList.isEmpty()) {
			return ""; //$NON-NLS-1$
		}
		Collections.sort(optList, new OptionComparator());

		final StringBuilder buffer = new StringBuilder();
		for (final Option option : optList) {
			buffer.append("| `"); //$NON-NLS-1$
			if (option.getOpt() == null) {
				buffer.append(CliUtilities.getCommandLineOption(option.getLongOpt()));
			} else {
				buffer.append(CliUtilities.getCommandLineOption(option.getOpt()));
				if (option.hasLongOpt()) {
					buffer.append("`, `"); //$NON-NLS-1$
					buffer.append(CliUtilities.getCommandLineOption(option.getLongOpt()));
				}
			}

			if (option.hasArg()) {
				if (option.hasArgName()) {
					buffer.append(" <").append(option.getArgName()).append(">"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}

			buffer.append("` | "); //$NON-NLS-1$

			if (option.getDescription() != null) {
				String text = option.getDescription().replaceAll("[ \t\n\r\f]+", " "); //$NON-NLS-1$ //$NON-NLS-2$
				text = text.replaceAll("\\<", "&lt;");  //$NON-NLS-1$//$NON-NLS-2$
				text = text.replaceAll("\\>", "&gt;");  //$NON-NLS-1$//$NON-NLS-2$
				buffer.append(text);
			}

			buffer.append(" |\n"); //$NON-NLS-1$
		}

		return buffer.toString();
	}

	/** Render the table to a Markdown table.
	 *
	 * @param table the table content.
	 * @return the markdown table.
	 */
	protected static String _renderToMarkdown(List<List<String>> table) {
		if (table == null || table.isEmpty()) {
			throw new MarkdownEmptyArrayException();
		}
		final StringBuilder buffer = new StringBuilder();
		for (final List<String> line : table) {
			buffer.append("| "); //$NON-NLS-1$
			if (line != null) {
				boolean first = true;
				for (final String column : line) {
					if (first) {
						first = false;
					} else {
						buffer.append(" | "); //$NON-NLS-1$
					}
					if (column != null) {
						String text = column;
						text = text.replaceAll("\\<", "&lt;");  //$NON-NLS-1$//$NON-NLS-2$
						text = text.replaceAll("\\>", "&gt;");  //$NON-NLS-1$//$NON-NLS-2$
						text = text.replaceAll("\n+", "<br>"); //$NON-NLS-1$ //$NON-NLS-2$
						text = text.replaceAll("[ \t\r\f]+", " "); //$NON-NLS-1$ //$NON-NLS-2$
						text = text.replaceAll(Pattern.quote("&p.lt;"), "<");  //$NON-NLS-1$//$NON-NLS-2$
						text = text.replaceAll(Pattern.quote("&p.gt;"), ">");  //$NON-NLS-1$//$NON-NLS-2$
						buffer.append(text);
					}
				}
			}
			buffer.append(" |\n"); //$NON-NLS-1$
		}
		return buffer.toString();
	}

	/** Comparator of command-line options.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static class OptionComparator implements Comparator<Option> {

		OptionComparator() {
			//
		}

		private static String getKey(Option opt) {
			final String val = opt.getLongOpt();
			if (val == null) {
				return opt.getOpt();
			}
			return val;
		}

		@Override
		public int compare(Option opt1, Option opt2) {
			return getKey(opt1).compareToIgnoreCase(getKey(opt2));
		}

	}

}
