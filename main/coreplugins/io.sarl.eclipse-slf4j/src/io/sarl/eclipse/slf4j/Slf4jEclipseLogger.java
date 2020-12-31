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

package io.sarl.eclipse.slf4j;

import java.text.MessageFormat;

import org.eclipse.core.runtime.ILog;
import org.slf4j.Logger;
import org.slf4j.Marker;

/**
 * SLF4J logger that is linked to the M2E plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @deprecated Remove when SLF4J is properly configured for Eclipse.
 */
@Deprecated
public class Slf4jEclipseLogger implements Logger {

	private final ILog logger;

	private final String name;

	/** Constructor.
	 *
	 * @param name the name of the logger.
	 * @param log the M2E logger.
	 */
	public Slf4jEclipseLogger(String name, ILog log) {
		this.name = name;
		this.logger = log;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public boolean isTraceEnabled() {
		return false;
	}

	@Override
	public boolean isTraceEnabled(Marker marker) {
		return false;
	}

	@Override
	public void trace(String msg) {
		//
	}

	@Override
	public void trace(String format, Object arg) {
		//
	}

	@Override
	public void trace(String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void trace(String format, Object... arguments) {
		//
	}

	@Override
	public void trace(String msg, Throwable t) {
		//
	}

	@Override
	public void trace(Marker marker, String msg) {
		//
	}

	@Override
	public void trace(Marker marker, String format, Object arg) {
		//
	}

	@Override
	public void trace(Marker marker, String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void trace(Marker marker, String format, Object... argArray) {
		//
	}

	@Override
	public void trace(Marker marker, String msg, Throwable t) {
		//
	}

	@Override
	public boolean isDebugEnabled() {
		return false;
	}

	@Override
	public boolean isDebugEnabled(Marker marker) {
		return false;
	}

	@Override
	public void debug(String msg) {
		//
	}

	@Override
	public void debug(String format, Object arg) {
		//
	}

	@Override
	public void debug(String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void debug(String format, Object... arguments) {
		//
	}

	@Override
	public void debug(String msg, Throwable t) {
		//
	}

	@Override
	public void debug(Marker marker, String msg) {
		//
	}

	@Override
	public void debug(Marker marker, String format, Object arg) {
		//
	}

	@Override
	public void debug(Marker marker, String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void debug(Marker marker, String format, Object... arguments) {
		//
	}

	@Override
	public void debug(Marker marker, String msg, Throwable t) {
		//
	}

	@Override
	public boolean isInfoEnabled() {
		return false;
	}

	@Override
	public boolean isInfoEnabled(Marker marker) {
		return false;
	}

	@Override
	public void info(String msg) {
		//
	}

	@Override
	public void info(String format, Object arg) {
		//
	}

	@Override
	public void info(String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void info(String format, Object... arguments) {
		//
	}

	@Override
	public void info(String msg, Throwable t) {
		//
	}

	@Override
	public void info(Marker marker, String msg) {
		//
	}

	@Override
	public void info(Marker marker, String format, Object arg) {
		//
	}

	@Override
	public void info(Marker marker, String format, Object arg1, Object arg2) {
		//
	}

	@Override
	public void info(Marker marker, String format, Object... arguments) {
		//
	}

	@Override
	public void info(Marker marker, String msg, Throwable t) {
		//
	}

	@Override
	public boolean isWarnEnabled() {
		return true;
	}

	@Override
	public boolean isWarnEnabled(Marker marker) {
		return true;
	}

	@Override
	public void warn(String msg) {
		this.logger.warn(msg);
	}

	@Override
	public void warn(String format, Object arg) {
		this.logger.warn(MessageFormat.format(format, arg));
	}

	@Override
	public void warn(String format, Object... arguments) {
		this.logger.warn(MessageFormat.format(format, arguments));
	}

	@Override
	public void warn(String format, Object arg1, Object arg2) {
		this.logger.warn(MessageFormat.format(format, arg1, arg2));
	}

	@Override
	public void warn(String msg, Throwable t) {
		this.logger.warn(msg, t);
	}

	@Override
	public void warn(Marker marker, String msg) {
		warn(msg);
	}

	@Override
	public void warn(Marker marker, String format, Object arg) {
		warn(format, arg);
	}

	@Override
	public void warn(Marker marker, String format, Object arg1, Object arg2) {
		warn(format, arg1, arg2);
	}

	@Override
	public void warn(Marker marker, String format, Object... arguments) {
		warn(format, arguments);
	}

	@Override
	public void warn(Marker marker, String msg, Throwable t) {
		warn(msg, t);
	}

	@Override
	public boolean isErrorEnabled() {
		return true;
	}

	@Override
	public boolean isErrorEnabled(Marker marker) {
		return true;
	}

	@Override
	public void error(String msg) {
		this.logger.error(msg);
	}

	@Override
	public void error(String format, Object arg) {
		this.logger.error(MessageFormat.format(format, arg));
	}

	@Override
	public void error(String format, Object arg1, Object arg2) {
		this.logger.error(MessageFormat.format(format, arg1, arg2));
	}

	@Override
	public void error(String format, Object... arguments) {
		this.logger.error(MessageFormat.format(format, arguments));
	}

	@Override
	public void error(String msg, Throwable t) {
		this.logger.error(msg, t);
	}

	@Override
	public void error(Marker marker, String msg) {
		error(msg);
	}

	@Override
	public void error(Marker marker, String format, Object arg) {
		error(format, arg);
	}

	@Override
	public void error(Marker marker, String format, Object arg1, Object arg2) {
		error(format, arg1, arg2);
	}

	@Override
	public void error(Marker marker, String format, Object... arguments) {
		error(format, arguments);
	}

	@Override
	public void error(Marker marker, String msg, Throwable t) {
		error(msg, t);
	}

}
