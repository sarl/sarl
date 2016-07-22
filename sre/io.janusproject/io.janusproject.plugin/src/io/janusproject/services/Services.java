/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.services;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.State;
import com.google.common.util.concurrent.ServiceManager;
import io.janusproject.services.infrastructure.InfrastructureService;
import org.arakhne.afc.vmutil.ClassComparator;

/**
 * Tools for launching and stopping services.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Services {

	private Services() {
		//
	}

	/**
	 * Start the services associated to the given service manager.
	 *
	 * <p>
	 * This starting function supports the {@link DependentService prioritized services}.
	 *
	 * @param manager - the manager of the services to start.
	 */
	public static void startServices(ServiceManager manager) {
		startServices(new GoogleServiceManager(manager));
	}

	/**
	 * Start the services associated to the given service manager.
	 *
	 * <p>
	 * This starting function supports the {@link DependentService prioritized services}.
	 *
	 * @param manager - the manager of the services to start.
	 */
	public static void startServices(IServiceManager manager) {
		List<Service> otherServices = new ArrayList<>();
		List<Service> infraServices = new ArrayList<>();
		LinkedList<DependencyNode> serviceQueue = new LinkedList<>();
		Accessors accessors = new StartingPhaseAccessors();

		// Build the dependency graph
		buildDependencyGraph(manager, serviceQueue, infraServices, otherServices, accessors);

		// Launch the services
		runDependencyGraph(serviceQueue, infraServices, otherServices, accessors);

		manager.awaitHealthy();
	}

	/**
	 * Stop the services associated to the given service manager.
	 *
	 * <p>
	 * This stopping function supports the {@link DependentService prioritized services}.
	 *
	 * @param manager - the manager of the services to stop.
	 */
	public static void stopServices(ServiceManager manager) {
		stopServices(new GoogleServiceManager(manager));
	}

	/**
	 * Stop the services associated to the given service manager.
	 *
	 * <p>
	 * This stopping function supports the {@link DependentService prioritized services}.
	 *
	 * @param manager - the manager of the services to stop.
	 */
	public static void stopServices(IServiceManager manager) {
		List<Service> otherServices = new ArrayList<>();
		List<Service> infraServices = new ArrayList<>();
		LinkedList<DependencyNode> serviceQueue = new LinkedList<>();
		Accessors accessors = new StoppingPhaseAccessors();

		// Build the dependency graph
		buildInvertedDependencyGraph(manager, serviceQueue, infraServices, otherServices, accessors);

		// Launch the services
		runDependencyGraph(serviceQueue, infraServices, otherServices, accessors);

		manager.awaitStopped();
	}

	private static void addNodeIntoDependencyGraph(DependentService depServ,
			Map<Class<? extends Service>, DependencyNode> dependentServices, List<DependencyNode> roots) {
		Class<? extends Service> type = depServ.getServiceType();
		assert (type != null);
		assert (type.isInterface()) : type.getName();
		DependencyNode node = dependentServices.get(type);
		if (node == null) {
			node = new DependencyNode(depServ, type);
			dependentServices.put(type, node);
		} else {
			assert (node.getService() == null);
			node.setService(depServ);
		}

		boolean isRoot = true;
		Collection<Class<? extends Service>> deps = depServ.getServiceDependencies();
		for (Class<? extends Service> dep : deps) {
			isRoot = false;
			DependencyNode depNode = dependentServices.get(dep);
			if (depNode == null) {
				depNode = new DependencyNode(dep);
				dependentServices.put(dep, depNode);
			}
			depNode.getNextServices().add(node);
		}

		deps = depServ.getServiceWeakDependencies();
		for (Class<? extends Service> dep : deps) {
			isRoot = false;
			DependencyNode depNode = dependentServices.get(dep);
			if (depNode == null) {
				depNode = new DependencyNode(dep);
				dependentServices.put(dep, depNode);
			}
			depNode.getNextWeakServices().add(node);
		}

		if (isRoot) {
			roots.add(node);
		}
	}

	/**
	 * Build the dependency graph for the services.
	 *
	 * @param manager - lsit of the services.
	 * @param roots - filled with the services that have no dependency.
	 * @param infraServices - filled with the infrastructure services.
	 * @param freeServices - filled with the services that are executed before/after all the dependent services.
	 * @param accessors - permits to retreive information on the services.
	 */
	private static void buildDependencyGraph(IServiceManager manager, List<DependencyNode> roots, List<Service> infraServices,
			List<Service> freeServices, Accessors accessors) {
		Map<Class<? extends Service>, DependencyNode> dependentServices = new TreeMap<>(ClassComparator.SINGLETON);

		Service service;
		for (Entry<State, Service> entry : manager.servicesByState().entries()) {
			if (accessors.matches(entry.getKey())) {
				service = entry.getValue();
				if (service instanceof InfrastructureService) {
					infraServices.add(service);
				} else if (service instanceof DependentService) {
					addNodeIntoDependencyGraph((DependentService) service, dependentServices, roots);
				} else {
					freeServices.add(service);
				}
			}
		}

		if (accessors.isAsyncStateWaitingEnabled()) {
			for (DependencyNode node : dependentServices.values()) {
				assert (node.getService() != null);
				if (node.getService() instanceof AsyncStateService) {
					for (DependencyNode next : node.getNextServices()) {
						next.getAsyncStateServices().add(new WeakReference<>(node));
					}
				}
			}
		}
	}

	/**
	 * Build the dependency graph for the services.
	 *
	 * @param manager - lsit of the services.
	 * @param roots - filled with the services that have no dependency.
	 * @param infraServices - filled with the infrastructure services.
	 * @param freeServices - filled with the services that are executed before/after all the dependent services.
	 * @param accessors - permits to retreive information on the services.
	 */
	private static void buildInvertedDependencyGraph(IServiceManager manager, List<DependencyNode> roots,
			List<Service> infraServices, List<Service> freeServices, Accessors accessors) {
		Map<Class<? extends Service>, DependencyNode> dependentServices = new TreeMap<>(ClassComparator.SINGLETON);
		Map<Class<? extends Service>, DependencyNode> rootServices = new TreeMap<>(ClassComparator.SINGLETON);

		Service service;
		for (Entry<State, Service> entry : manager.servicesByState().entries()) {
			if (accessors.matches(entry.getKey())) {
				service = entry.getValue();
				if (service instanceof InfrastructureService) {
					infraServices.add(service);
				} else if (service instanceof DependentService) {
					DependentService depServ = (DependentService) service;
					Class<? extends Service> type = depServ.getServiceType();
					DependencyNode node = dependentServices.get(type);
					boolean isRoot = true;
					if (node == null) {
						node = new DependencyNode(depServ, type);
						dependentServices.put(type, node);
					} else {
						assert (node.getService() == null);
						node.setService(depServ);
						isRoot = false;
					}

					Collection<Class<? extends Service>> deps = depServ.getServiceDependencies();
					for (Class<? extends Service> dep : deps) {
						DependencyNode depNode = dependentServices.get(dep);
						if (depNode == null) {
							depNode = new DependencyNode(dep);
							dependentServices.put(dep, depNode);
						}
						node.getNextServices().add(depNode);
						rootServices.remove(depNode.getType());
					}

					deps = depServ.getServiceWeakDependencies();
					for (Class<? extends Service> dep : deps) {
						DependencyNode depNode = dependentServices.get(dep);
						if (depNode == null) {
							depNode = new DependencyNode(dep);
							dependentServices.put(dep, depNode);
						}
						node.getNextWeakServices().add(depNode);
						rootServices.remove(depNode.getType());
					}

					if (isRoot) {
						rootServices.put(type, node);
					}
				} else {
					freeServices.add(service);
				}
			}
		}

		roots.addAll(rootServices.values());
	}

	/**
	 * Run the dependency graph for the services.
	 *
	 * @param roots - filled with the services that have no dependency.
	 * @param infraServices - filled with the infrastructure services.
	 * @param freeServices - filled with the services that are executed before/after all the dependent services.
	 * @param accessors - permits to retreive information on the services.
	 */
	private static void runDependencyGraph(Queue<DependencyNode> roots, List<Service> infraServices, List<Service> freeServices,
			Accessors accessors) {
		final boolean async = accessors.isAsyncStateWaitingEnabled();
		Set<Class<? extends Service>> executed = new TreeSet<>(ClassComparator.SINGLETON);
		accessors.runInfrastructureServicesBefore(infraServices);
		accessors.runFreeServicesBefore(freeServices);
		while (!roots.isEmpty()) {
			DependencyNode node = roots.remove();
			assert (node != null && node.getType() != null);
			if (!executed.contains(node.getType())) {
				executed.add(node.getType());
				roots.addAll(node.getNextServices());
				roots.addAll(node.getNextWeakServices());
				assert (node.getService() != null);
				if (async) {
					for (WeakReference<DependencyNode> asyncService : node.getAsyncStateServices()) {
						AsyncStateService as = (AsyncStateService) (asyncService.get().getService());
						assert (as != null);
						while (!as.isReadyForOtherServices()) {
							Thread.yield();
						}
					}
				}
				accessors.run(node.getService());
			}
		}
		accessors.runFreeServicesAfter(freeServices);
		accessors.runInfrastructureServicesAfter(infraServices);
	}

	/**
	 * Node that describes a service dependency.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class DependencyNode {

		private Service service;

		private final Class<? extends Service> type;

		private final Collection<DependencyNode> nextServices = new ArrayList<>();

		private final Collection<DependencyNode> nextWeakServices = new ArrayList<>();

		private final Collection<WeakReference<DependencyNode>> asyncStateServices = new ArrayList<>();

		DependencyNode(DependentService service, Class<? extends Service> type) {
			assert (service != null);
			this.service = service;
			this.type = type;
		}

		DependencyNode(Class<? extends Service> type) {
			this.service = null;
			this.type = type;
		}

		public Service getService() {
			return this.service;
		}

		public void setService(Service service) {
			this.service = service;
		}

		public Class<? extends Service> getType() {
			return this.type;
		}

		public Collection<WeakReference<DependencyNode>> getAsyncStateServices() {
			return this.asyncStateServices;
		}

		public Collection<DependencyNode> getNextServices() {
			return this.nextServices;
		}

		public Collection<DependencyNode> getNextWeakServices() {
			return this.nextWeakServices;
		}

		@Override
		public String toString() {
			if (this.service == null) {
				return "!!!" + this.type.getName(); //$NON-NLS-1$
			}
			return this.service.toString();
		}

	}

	/**
	 * Accessors for running services.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private interface Accessors {

		boolean matches(State element);

		void runInfrastructureServicesBefore(List<Service> infraServices);

		void runFreeServicesBefore(List<Service> freeServices);

		boolean isAsyncStateWaitingEnabled();

		void run(Service service);

		void runFreeServicesAfter(List<Service> freeServices);

		void runInfrastructureServicesAfter(List<Service> infraServices);

	}

	/**
	 * Accessors for running services at start-up.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class StartingPhaseAccessors implements Accessors {

		/**
		 * Construct.
		 */
		StartingPhaseAccessors() {
			//
		}

		@Override
		public boolean matches(State element) {
			return element == State.NEW;
		}

		@Override
		public void runFreeServicesBefore(List<Service> freeServices) {
			//
		}

		@Override
		public void runInfrastructureServicesBefore(List<Service> infraServices) {
			for (Service serv : infraServices) {
				serv.startAsync().awaitRunning();
			}
		}

		@Override
		public boolean isAsyncStateWaitingEnabled() {
			return true;
		}

		@Override
		public void run(Service service) {
			service.startAsync().awaitRunning();
		}

		@Override
		public void runFreeServicesAfter(List<Service> freeServices) {
			for (Service serv : freeServices) {
				serv.startAsync();
			}
		}

		@Override
		public void runInfrastructureServicesAfter(List<Service> infraServices) {
			//
		}

	}

	/**
	 * Accessors for running agents at end.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class StoppingPhaseAccessors implements Accessors {

		/**
		 * Construct.
		 */
		StoppingPhaseAccessors() {
			//
		}

		@Override
		public boolean matches(State element) {
			return element != State.TERMINATED && element != State.STOPPING;
		}

		@Override
		public void runFreeServicesBefore(List<Service> freeServices) {
			for (Service serv : freeServices) {
				serv.stopAsync();
			}
		}

		@Override
		public void runInfrastructureServicesBefore(List<Service> infraServices) {
			//
		}

		@Override
		public boolean isAsyncStateWaitingEnabled() {
			return false;
		}

		@Override
		public void run(Service service) {
			service.stopAsync().awaitTerminated();
		}

		@Override
		public void runFreeServicesAfter(List<Service> freeServices) {
			//
		}

		@Override
		public void runInfrastructureServicesAfter(List<Service> infraServices) {
			for (Service serv : infraServices) {
				serv.stopAsync();
			}
		}

	}

}
