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

package io.sarl.eclipse.slf4j;

import java.text.MessageFormat;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.LegacyAbstractLogger;

/** Apache logger that is wrapping an Eclipse logger.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class EclipseSlf4jLogger extends LegacyAbstractLogger {

	private static final long serialVersionUID = -5538811471324622933L;

	private final ILog eclipseLogger;

	/** Constructor.
	 *
	 * @param name the name of the logger.
	 * @param eclipseLogger the logger.
	 */
	EclipseSlf4jLogger(String name, ILog eclipseLogger) {
		this.name = name;
		this.eclipseLogger = eclipseLogger;
	}

	@Override
	public boolean isTraceEnabled() {
		return false;
	}

	@Override
	public boolean isDebugEnabled() {
		return false;
	}

	@Override
	public boolean isInfoEnabled() {
		return true;
	}

	@Override
	public boolean isWarnEnabled() {
		return true;
	}

	@Override
	public boolean isErrorEnabled() {
		return true;
	}

	@Override
	protected String getFullyQualifiedCallerName() {
		return null;
	}

	private static IStatus createStatus(Level level, String msg, Object[] arguments, Throwable throwable) {
		IStatus status = null;
		if (level != null) {
			final int istatus;
			switch (level) {
			case INFO:
				istatus = IStatus.INFO;
				break;
			case WARN:
				istatus = IStatus.WARNING;
				break;
			case ERROR:
				istatus = IStatus.ERROR;
				break;
			case DEBUG:
			case TRACE:
			default:
				istatus = 0;
				break;
			}
			if (istatus != 0) {
				final String expandedMessage = MessageFormat.format(msg, arguments);
				if (throwable != null) {
					status = new Status(istatus, EclipseSLF4JLoggerPlugin.PLUGIN_ID, expandedMessage, throwable);
				} else {
					status = new Status(istatus, EclipseSLF4JLoggerPlugin.PLUGIN_ID, expandedMessage);
				}
			}
		}
		return status;
	}

	@Override
	protected void handleNormalizedLoggingCall(Level level, Marker marker, String msg, Object[] arguments,
			Throwable throwable) {
		final IStatus status = createStatus(level, msg, arguments, throwable);
		if (status != null) {
			this.eclipseLogger.log(status);
		}
	}

}
