/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.janusproject.kernel.services.jdk.logging;

import java.util.Collection;
import java.util.Collections;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.util.concurrent.AbstractService;
import com.google.common.util.concurrent.Service;

import io.janusproject.services.logging.LogService;

/**
 * This class provides an implementation of the {@link LogService} that outputs nothing.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EmptyLogService extends AbstractService implements LogService {

	private final Logger logger;

	/**
	 * Construct.
	 */
	public EmptyLogService() {
		this.logger = Logger.getLogger(EmptyLogService.class.getName());
	}

	@Override
	public Class<? extends Service> getServiceType() {
		return LogService.class;
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Collections.emptyList();
	}

	@Override
	public Collection<Class<? extends Service>> getServiceWeakDependencies() {
		return Collections.emptyList();
	}

	@Override
	public void info(String message, Object... params) {
		//
	}

	@Override
	public void fineInfo(String message, Object... params) {
		//
	}

	@Override
	public void finerInfo(String message, Object... params) {
		//
	}

	@Override
	public void debug(String message, Object... params) {
		//
	}

	@Override
	public void warning(String message, Object... params) {
		//
	}

	@Override
	public void error(String message, Object... params) {
		//
	}

	@Override
	public void log(LogRecord record) {
		//
	}

	@Override
	public void log(Level level, String message, Object... params) {
		//
	}

	@Override
	public Logger getLogger() {
		return this.logger;
	}

	@Override
	public void setLogger(Logger logger) {
		//
	}

	@Override
	public void setFilter(Filter filter) {
		//
	}

	@Override
	public Filter getFilter() {
		return null;
	}

	@Override
	public boolean isLoggeable(Level level) {
		return false;
	}

	@Override
	public Level getLevel() {
		return Level.OFF;
	}

	@Override
	public void setLevel(Level level) {
		//
	}

	@Override
	protected void doStart() {
		this.logger.setLevel(Level.OFF);
		notifyStarted();
	}

	@Override
	protected void doStop() {
		this.logger.setLevel(Level.OFF);
		notifyStopped();
	}

}
