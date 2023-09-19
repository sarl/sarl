/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2022 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.arakhne.afc.bootique.synopsishelp.help;

import java.lang.reflect.Method;

import com.google.common.base.Strings;
import io.bootique.help.ConsoleAppender;
import io.bootique.help.DefaultHelpGenerator;
import io.bootique.help.HelpAppender;
import io.bootique.meta.application.ApplicationMetadata;

/** A generator of command-line help that displays the synopsis in addition to
 * the other sections provided by {@link DefaultHelpGenerator}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
public class SynopsisHelpGenerator extends DefaultHelpGenerator {

	private static final String SYNOPSIS = "SYNOPSIS"; //$NON-NLS-1$

	private static final String DETAILED_DESCRIPTION = "DESCRIPTION"; //$NON-NLS-1$

	private static final String OPTION_SYNOPSIS = " [OPTIONS]... "; //$NON-NLS-1$

	private static final String ARGUMENT_SYNOPSIS = " [ARGUMENT]... "; //$NON-NLS-1$

	private final ApplicationMetadata metadata;

	private final String argumentSynopsis;

	private final String detailedDescription;

	/** Constructor.
	 *
	 * @param metadata the metadata of the application.
	 * @param argumentSynopsis the synopsis of the arguments. If {@code null}, the default description is used.
	 *     If it is an empty string, no argument description is displayed.
	 * @param detailedDescription the detailed description of the application.
	 * @param lineWidth the width of a console line.
	 */
	public SynopsisHelpGenerator(ApplicationMetadata metadata, String argumentSynopsis,
			String detailedDescription, int lineWidth) {
		super(metadata, lineWidth);
		this.metadata = metadata;
		this.detailedDescription = detailedDescription;
		if (argumentSynopsis == null) {
			this.argumentSynopsis = ARGUMENT_SYNOPSIS;
		} else {
			this.argumentSynopsis = argumentSynopsis;
		}
	}

	@Override
	protected SynopsisHelpAppender createAppender(Appendable out) {
		return new SynopsisHelpAppender(createConsoleAppender(out));
	}

	/** Replies the description of the application.
	 *
	 * @return the metadata.
	 */
	protected ApplicationMetadata getApplicationMetadata() {
		return this.metadata;
	}

	@Override
	public void append(Appendable out) {
		final SynopsisHelpAppender appender = createAppender(out);
		final ApplicationMetadata meta = getApplicationMetadata();
		final String name = meta.getName();
		printName(appender, name, meta.getDescription());
		printSynopsis(appender, name, this.argumentSynopsis);
		printDetailedDescription(appender, this.detailedDescription);
		printOptions(appender, collectOptions());
		printEnvironment(appender, meta.getVariables());
	}

	/** Print the synopsis of the command.
	 *
	 * @param out the output receiver.
	 * @param name the name of the command.
	 * @param argumentSynopsis the synopsis of the arguments.
	 */
	@SuppressWarnings("static-method")
	protected void printSynopsis(HelpAppender out, String name, String argumentSynopsis) {
		out.printSectionName(SYNOPSIS);

		assert name != null;

		if (Strings.isNullOrEmpty(argumentSynopsis)) {
			out.printText(name, OPTION_SYNOPSIS);
		} else {
			out.printText(name, OPTION_SYNOPSIS, argumentSynopsis);
		}
	}

	/** Print the detailed description of the command.
	 *
	 * @param out the output receiver.
	 * @param detailedDescription the detailed description of the application.
	 */
	@SuppressWarnings("static-method")
	protected void printDetailedDescription(SynopsisHelpAppender out, String detailedDescription) {
		if (!Strings.isNullOrEmpty(detailedDescription)) {
			out.printSectionName(DETAILED_DESCRIPTION);
			out.printLongDescription(detailedDescription.split("[\r\n\f]+")); //$NON-NLS-1$
		}
	}

	/** Appender for the synopsis help.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 15.0
	 */
	protected static class SynopsisHelpAppender extends HelpAppender {

		private transient ConsoleAppender offsetAppender;

		private transient ConsoleAppender doubleOffsetAppender;

		/** Constructor.
		 *
		 * @param appender the console appender.
		 */
		public SynopsisHelpAppender(ConsoleAppender appender) {
			super(appender);
		}

		/** Get or create an appender with a single offset.
		 *
		 * @return the appender.
		 * @throws Error in case of internal error.
		 */
		protected ConsoleAppender getOrCreateOffsetAppender() {
			if (this.offsetAppender == null) {
				try {
					final Method method = HelpAppender.class.getDeclaredMethod("getOrCreateOffsetAppender"); //$NON-NLS-1$
					method.setAccessible(true);
					this.offsetAppender = (ConsoleAppender) method.invoke(this);
				} catch (Throwable exception) {
					throw new Error(exception);
				}
			}

			return this.offsetAppender;
		}

		/** Get or create an appender with a single offset.
		 *
		 * @return the appender.
		 * @throws Error in case of internal error.
		 */
		protected ConsoleAppender getOrCreateDoubleOffsetAppender() {
			if (this.doubleOffsetAppender == null) {
				try {
					final Method method = HelpAppender.class.getDeclaredMethod("getOrCreateDoubleOffsetAppender"); //$NON-NLS-1$
					method.setAccessible(true);
					this.doubleOffsetAppender = (ConsoleAppender) method.invoke(this);
				} catch (Throwable exception) {
					throw new Error(exception);
				}
			}

			return this.doubleOffsetAppender;
		}

		/** Print a long description. Each part is a different paragraph.
		 *
		 * @param parts the parts to print out.
		 */
		public void printLongDescription(String... parts) {
			final ConsoleAppender appender = getOrCreateOffsetAppender();
			boolean first = true;
			for (final String paragraph : parts) {
				if (first) {
					first = false;
				} else {
					appender.println(""); //$NON-NLS-1$
				}
				appender.foldPrintln(paragraph);
			}
		}

	}

}
