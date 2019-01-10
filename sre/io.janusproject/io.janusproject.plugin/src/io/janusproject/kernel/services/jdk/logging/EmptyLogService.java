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

package io.janusproject.kernel.services.jdk.logging;

import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.util.concurrent.AbstractService;
import com.google.common.util.concurrent.Service;

import io.janusproject.services.logging.LogService;
import io.janusproject.util.LoggerCreator;

/**
 * This class provides an implementation of the {@link LogService} that outputs nothing.
 *
 * <p>This service is thread-safe.
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
		this.logger = LoggerCreator.createPlatformLogger();
		this.logger.setLevel(Level.OFF);
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
	protected void doStart() {
		this.logger.setLevel(Level.OFF);
		notifyStarted();
	}

	@Override
	protected void doStop() {
		this.logger.setLevel(Level.OFF);
		notifyStopped();
	}

	@Override
	public Logger getPlatformLogger() {
		return this.logger;
	}

	@Override
	public Logger getKernelLogger() {
		return this.logger;
	}

	@Override
	public LogRecord prepareLogRecord(LogRecord record, String loggerName, Throwable exception) {
		return record;
	}

}
