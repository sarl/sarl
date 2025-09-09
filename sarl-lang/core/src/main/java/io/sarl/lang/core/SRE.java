/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core;

import java.lang.ref.SoftReference;
import java.net.URL;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.ServiceLoader;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Represents an access point to the SARL run-time environment (SRE).
 * This access point may be used for accessing the underlying SRE independently of its implementation.
 *
 * <p>Depending on the implementation of the SRE, an instance of this SRE access point could be injected.
 *
 * <p>For declaring a SRE bootstrap, the library that contains the contributing SRE must declared
 * a specific service implementation of {@link SRE}. The declaration of this service must be
 * done by creating a file into the folder {@code META-INF/services/io.sarl.lang.core.SREBootstrap}.
 * This file contains a single line that is the fully qualified name of the {@link SRE}'s implementation.
 *
 * @author $Author: sgalland$
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.6
 * @see ServiceLoader
 */
public final class SRE {

	private static final String PREFIX = "META-INF/services/"; //$NON-NLS-1$

	private static SREBootstrap currentSRE;

	private static SoftReference<ServiceLoader<SREBootstrap>> loader;

	private SRE() {
		//
	}

	/** Reset the service loader for the SRE bootstrap.
	 */
	public static void resetServiceLoader() {
		synchronized (SRE.class) {
			loader = null;
		}
	}

	/** Replies all the installed SRE into the class path.
	 *
	 * @return the installed SRE.
	 */
	@Pure
	@Inline("getServiceLoader(false)")
	public static ServiceLoader<SREBootstrap> getServiceLoader() {
		return getServiceLoader(false);
	}

	/** Replies all the installed SRE into the class path.
	 *
	 * @param onlyInstalledInJRE indicates if the services will be considered only into the libraries that are
	 *     installed into the JRE. If {@code true}, only the libraries into the JRE will be considered and
	 *     the application libraries will be ignored. If {@code false}, the application libraries will be
	 *     considered as well.
	 * @return the installed SRE.
	 */
	@Pure
	public static ServiceLoader<SREBootstrap> getServiceLoader(boolean onlyInstalledInJRE) {
		synchronized (SRE.class) {
			var sl = loader == null ? null : loader.get();
			if (sl == null) {
			    if (onlyInstalledInJRE) {
					sl = ServiceLoader.loadInstalled(SREBootstrap.class);
				} else {
					sl = ServiceLoader.load(SREBootstrap.class);
				}
				loader = new SoftReference<>(sl);
			}
			return sl;
		}
	}

	/** Replies all the libraries that contains a SRE bootstrap.
	 *
	 * @return the set of libraries.
	 * @since 0.7
	 */
	@Pure
	@Inline(value = "getServiceLibraries($1.class.getName())", imported = {SREBootstrap.class})
	public static Iterable<URL> getBootstrappedLibraries() {
		return getServiceLibraries(SREBootstrap.class.getName());
	}


	/** Replies all the libraries that contains a Java service for the given service name.
	 *
	 * @param libraryName the name of the service that is the fully qualified name of the service class.
	 * @return the set of libraries.
	 * @since 0.12
	 */
	@Pure
	public static Iterable<URL> getServiceLibraries(String libraryName) {
		final var name = PREFIX + libraryName;
        return () -> {
        	try {
	    		final Enumeration<URL> enumr = ClassLoader.getSystemResources(name);
	    		return new Iterator<>() {
	    			final Enumeration<URL> enumeration = enumr;

	    			@Override
	    			public boolean hasNext() {
	    				return this.enumeration.hasMoreElements();
	    			}

	    			@Override
	    			public URL next() {
	    				return this.enumeration.nextElement();
	    			}
	    		};
        	} catch (Throwable exception) {
        		return new Iterator<>() {
	    			@Override
	    			public boolean hasNext() {
	    				return false;
	    			}

	    			@Override
	    			public URL next() {
	    				throw new NoSuchElementException();
	    			}
	    		};
        	}
        };
	}

	/** Change the current SRE.
	 *
	 * @param sre the current SRE.
	 */
	public static void setBootstrap(SREBootstrap sre) {
		synchronized (SRE.class) {
			currentSRE = sre;
		}
	}

	/** Launch the SRE from a declared bootstrap within the JRE services.
	 * This execution entry point does not provide advanced interface, e.g. command line options.
	 *
	 * @param args the command-line arguments. The first argument must be the qualified name of the agent to be launched.
	 *     The other values are directly given to the launched agent as initialization parameter.
	 * @throws Exception in case of error.
	 * @since 0.7
	 */
	@SuppressWarnings("unchecked")
	public static void main(String[] args) throws Exception {
		final var params = new Object[args.length - 1];
		if (args.length > 1) {
			System.arraycopy(args, 1, params, 0, params.length);
		}
		final var type = (Class<? extends Agent>) SREClassLoader.loadClass(args[0],
				SRE.class.getClassLoader());
		getBootstrap().startAgent(type, params);
	}

	/** Find and reply the current SRE.
	 *
	 * @return the current SRE, never {@code null}.
	 * @throws IllegalStateException if a SRE cannot be found.
	 */
	@Pure
	public static SREBootstrap getBootstrap() {
		synchronized (SRE.class) {
			if (currentSRE == null) {
				final var iterator = getServiceLoader().iterator();
				if (iterator.hasNext()) {
					currentSRE = iterator.next();
				} else {
					currentSRE = new VoidSREBootstrap();
				}
			}
			return currentSRE;
		}
	}

	/**
	 * Private API: not documented.
	 *
	 * @author $Author: sgalland$
	 * @version core 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid core
	 * @since 0.6
	 * @ExcludeFromApidoc
	 */
	private static class VoidSREBootstrap implements SREBootstrap {

		/**
		 * Constructor.
		 */
		VoidSREBootstrap() {
			//
		}

		@Override
		public AgentContext startWithoutAgent(boolean asCommandLineApp) {
			return null;
		}

		@Override
		public void startAgent(Class<? extends Agent> agentCls, Object... params) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void startAgent(int nbAgents, Class<? extends Agent> agentCls, Object... params) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void startAgentWithID(Class<? extends Agent> agentCls, UUID agentID, Object... params) throws Exception {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean isActive() {
			return false;
		}

		@Override
		public boolean isRunning() {
			return false;
		}

		@Override
		public <T> T getService(Class<T> serviceType) {
			return null;
		}

		@Override
		public void shutdown(int timeout) throws InterruptedException {
			//
		}

		@Override
		public void addSREListener(SREListener listener) {
			//
		}

		@Override
		public void removeSREListener(SREListener listener) {
			//
		}

		@Override
		public void setCommandLineArguments(String[] arguments) {
			//
		}

		@Override
		public void injectMembers(Object object) {
			throw new UnsupportedOperationException();
		}

	}

}
