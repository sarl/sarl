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

import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.eclipse.core.runtime.ILog;
import org.slf4j.ILoggerFactory;
import org.slf4j.Logger;

/** Factory of logger.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class EclipseSlf4jLoggerFactory implements ILoggerFactory {

	private final ReadWriteLock lock = new ReentrantReadWriteLock();

	private ILog wrappedLogger;

	/** Constructor.
	 */
	public EclipseSlf4jLoggerFactory() {
		//
	}

	/** Replies the Eclipse logger.
	 *
	 * @return the logger.
	 */
	protected ILog getEclipseLogger() {
		ILog log;
		this.lock.readLock().lock();
		try {
			log = this.wrappedLogger;
		} finally {
			this.lock.readLock().unlock();
		}
		if (log == null) {
			this.lock.writeLock().lock();
			try {
				log = this.wrappedLogger;
				if (log == null) {
					try {
						this.wrappedLogger = EclipseSLF4JLoggerPlugin.getDefault().getLog();
					} catch (Throwable exception) {
						// This case may append when a SLF4J logger is requested before the
						// Eclipse platform is fully launched
					}
					log = this.wrappedLogger;
				}
			} finally {
				this.lock.writeLock().unlock();
			}
		}
		return log;
	}

	@Override
	public Logger getLogger(String name) {
		final ILog eclipseLogger = getEclipseLogger();
		if (eclipseLogger == null) {
			throw new IllegalStateException("Eclipse platfom is not fully launch. Logger cannot be retreived."); //$NON-NLS-1$
		}
		return new EclipseSlf4jLogger(name, eclipseLogger);
	}

}
