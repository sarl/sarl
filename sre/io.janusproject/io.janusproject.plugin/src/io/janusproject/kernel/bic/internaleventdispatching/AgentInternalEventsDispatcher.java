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

package io.janusproject.kernel.bic.internaleventdispatching;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.stream.StreamSupport;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.arakhne.afc.util.MultiCollection;
import org.arakhne.afc.util.OutputParameter;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.janusproject.services.executor.EarlyExitException;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.executor.JanusRunnable;

import io.sarl.eventdispatching.BehaviorGuardEvaluator;
import io.sarl.eventdispatching.BehaviorGuardEvaluatorRegistry;
import io.sarl.lang.core.Event;

/**
 * The class in charge of dispatching every single events coming from the outside of this agent (i.e. from a space) or from an
 * agent's behavior.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 *
 */
public class AgentInternalEventsDispatcher {

	/**
	 * The registry of all {@code BehaviorGuardEvaluator} classes containing a method to evaluate the guard of a given behavior
	 * (on clause in SARL behavior). This class has been inspired by the com.google.common.eventbus.SuscriberRegistry class of
	 * Google Guava library.
	 */
	private final BehaviorGuardEvaluatorRegistry behaviorGuardEvaluatorRegistry;

	/**
	 * The executor used to execute behavior methods in dedicated thread.
	 */
	private final ExecutorService executor;

	/**
	 * Instantiates a dispatcher.
	 *
	 * @param executor the executor service.
	 */
	@Inject
	public AgentInternalEventsDispatcher(ExecutorService executor) {
		this.executor = executor;
		this.behaviorGuardEvaluatorRegistry = new BehaviorGuardEvaluatorRegistry();
	}

	/** Replies if a listener with the given type is registered.
	 *
	 * @param type the type of listener.
	 * @return {@code true} if a listener of the given type is registered.
	 * @since 0.5
	 */
	public boolean hasRegisteredEventListener(Class<?> type) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			return this.behaviorGuardEvaluatorRegistry.hasRegisteredEventListener(type);
		}
	}

	/** Extract the registered listeners with the given type.
	 *
	 * @param <T> the type of the listeners.
	 * @param type the type of the listeners.
	 * @param collection the collection of listeners that is filled by this function.
	 * @return the number of listeners added to the collection.
	 * @since 0.5
	 */
	public <T> int getRegisteredEventListeners(Class<T> type, Collection<? super T> collection) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			return this.behaviorGuardEvaluatorRegistry.getRegisteredEventListeners(type, collection);
		}
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on {@code object} to receive events.
	 *
	 * <p>If the filter is provided, it will be used for determining if the given behavior accepts a specific event.
	 * If the filter function replies {@code true} for a specific event as argument, the event is fired in the
	 * behavior context. If the filter function replies {@code false}, the event is not fired in the behavior context.
	 *
	 * @param object object whose {@code PerceptGuardEvaluator} methods should be registered.
	 * @param filter the filter function. It could be {@code null}.
	 * @param callback function which is invoked just after the first registration of the object. It could be {@code null}.
	 */
	public void register(Object object, Function1<? super Event, ? extends Boolean> filter, Procedure1<? super Object> callback) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			this.behaviorGuardEvaluatorRegistry.register(object, filter, callback);
		}
	}

	/**
	 * Unregisters all {@code PerceptGuardEvaluator} methods on a registered {@code object}.
	 *
	 * @param object object whose {@code PerceptGuardEvaluator} methods should be unregistered.
	 * @param callback function which is invoked just before the object is unregistered.
	 * @throws IllegalArgumentException if the object was not previously registered.
	 */
	public void unregister(Object object, Procedure1<? super Object> callback) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			this.behaviorGuardEvaluatorRegistry.unregister(object, callback);
		}
	}

	/**
	 * Unregisters all {@code PerceptGuardEvaluator} methods on all registered objects.
	 *
	 * @param callback function which is invoked just before the object is unregistered.
	 * @throws IllegalArgumentException if the object was not previously registered.
	 */
	public void unregisterAll(Procedure1<? super Object> callback) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			this.behaviorGuardEvaluatorRegistry.unregisterAll(callback);
		}
	}

	/**
	 * Posts an event to all registered {@code BehaviorGuardEvaluator}.
	 * The dispatch of this event will be done synchronously.
	 * This method will return successfully after the event has been posted to all {@code BehaviorGuardEvaluator}, and regardless
	 * of any exceptions thrown by {@code BehaviorGuardEvaluator}.
	 *
	 * @param event an event to dispatch synchronously.
	 */
	public void immediateDispatch(Event event) {
		assert event != null;
		Iterable<BehaviorGuardEvaluator> behaviorGuardEvaluators = null;
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			behaviorGuardEvaluators = AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry
					.getBehaviorGuardEvaluators(event);
		}
		if (behaviorGuardEvaluators != null) {
			final Collection<Runnable> behaviorsMethodsToExecute;
			try {
				behaviorsMethodsToExecute = evaluateGuards(event, behaviorGuardEvaluators);
				executeBehaviorMethodsInParalellWithSynchroAtTheEnd(behaviorsMethodsToExecute);
			} catch (RuntimeException exception) {
				throw exception;
			} catch (InterruptedException | ExecutionException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}

		}
	}

	/**
	 * Posts an event to the registered {@code BehaviorGuardEvaluator} of the given listener only.
	 * The dispatch of this event will be done synchronously.
	 * This method will return successfully after the event has been posted to all {@code BehaviorGuardEvaluator}, and regardless
	 * of any exceptions thrown by {@code BehaviorGuardEvaluator}.
	 *
	 * @param listener the listener to dispatch to.
	 * @param event an event to dispatch synchronously.
	 */
	public void immediateDispatchTo(Object listener, Event event) {
		assert event != null;
		Iterable<BehaviorGuardEvaluator> behaviorGuardEvaluators = null;
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			behaviorGuardEvaluators = AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry
					.getBehaviorGuardEvaluatorsFor(event, listener);
		}
		if (behaviorGuardEvaluators != null) {
			final Collection<Runnable> behaviorsMethodsToExecute;
			try {
				behaviorsMethodsToExecute = evaluateGuards(event, behaviorGuardEvaluators);
				executeBehaviorMethodsInParalellWithSynchroAtTheEnd(behaviorsMethodsToExecute);
			} catch (RuntimeException exception) {
				throw exception;
			} catch (InterruptedException | ExecutionException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}

		}
	}

	/**
	 * Posts an event to all registered {@code BehaviorGuardEvaluator}.
	 * The dispatch of this event will be done asynchronously.
	 * This method will return successfully after the event has been posted to all {@code BehaviorGuardEvaluator}, and regardless
	 * of any exceptions thrown by {@code BehaviorGuardEvaluator}.
	 *
	 * @param event an event to dispatch asynchronously.
	 */
	public void asyncDispatch(Event event) {
		assert event != null;
		this.executor.execute(() -> {
			Iterable<BehaviorGuardEvaluator> behaviorGuardEvaluators = null;
			synchronized (AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry) {
				behaviorGuardEvaluators = AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry
						.getBehaviorGuardEvaluators(event);
			}
			if (behaviorGuardEvaluators != null) {
				final Collection<Runnable> behaviorsMethodsToExecute;
				try {
					behaviorsMethodsToExecute = evaluateGuards(event, behaviorGuardEvaluators);
				} catch (InvocationTargetException e) {
					throw new RuntimeException(e);
				}
				executeAsynchronouslyBehaviorMethods(behaviorsMethodsToExecute);
			}
		});
	}

	/**
	 * Evaluate the guard associated to the specified {@code event} and returns the list of behaviors methods that must be
	 * executed.
	 *
	 * @param event the event triggering behaviors
	 * @param behaviorGuardEvaluators the list of class containing a {@code PerceptGuardEvaluator} method
	 * @return the collection of couple associating a object and its collection of behavior methods that must be executed
	 * @throws InvocationTargetException - exception when you try to execute a method by reflection and this method doesn't exist.
	 */
	private static Collection<Runnable> evaluateGuards(final Event event,
			final Iterable<BehaviorGuardEvaluator> behaviorGuardEvaluators) throws InvocationTargetException {

		final MultiCollection<Runnable> behaviorsMethodsToExecute = new MultiCollection<>();

		try {
			StreamSupport.stream(behaviorGuardEvaluators.spliterator(), true).forEach(evaluator -> {
				final Collection<Runnable> behaviorsMethodsToExecutePerTarget = Lists.newLinkedList();
				evaluator.evaluateGuard(event, behaviorsMethodsToExecutePerTarget);
				synchronized (behaviorsMethodsToExecute) {
					behaviorsMethodsToExecute.addCollection(behaviorsMethodsToExecutePerTarget);
				}
			});
		} catch (Exception exception) {
			if (exception instanceof InvocationTargetException) {
				throw (InvocationTargetException) exception;
			}
			final Throwable t = exception.getCause();
			if (t instanceof InvocationTargetException) {
				throw (InvocationTargetException) t;
			}
			throw exception;
		}

		return behaviorsMethodsToExecute;
	}

	/**
	 * Execute every single Behaviors runnable, a dedicated thread will created by the executor local to this class and be used to
	 * execute each runnable in parallel, and this method waits until its future has been completed before leaving.
	 *
	 * <p>This function may fail if one of the called handlers has failed. Errors are logged by the executor service too.
	 *
	 * @param behaviorsMethodsToExecute the collection of Behaviors runnable that must be executed.
	 * @throws InterruptedException - something interrupt the waiting of the event handler terminations.
	 * @throws ExecutionException - when the event handlers cannot be called; or when one of the event handler has failed during
	 *     its run.
	 */
	private void executeBehaviorMethodsInParalellWithSynchroAtTheEnd(Collection<Runnable> behaviorsMethodsToExecute)
			throws InterruptedException, ExecutionException {

		final CountDownLatch doneSignal = new CountDownLatch(behaviorsMethodsToExecute.size());

		final OutputParameter<Throwable> runException = new OutputParameter<>();

		for (final Runnable runnable : behaviorsMethodsToExecute) {
			this.executor.execute(new JanusRunnable() {
				@Override
				public void run() {
					try {
						runnable.run();
					} catch (EarlyExitException e) {
						// Ignore this exception
					} catch (RuntimeException e) {
						// Catch exception for notifying the caller
						runException.set(e);
						// Do the standard behavior too -> logging
						throw e;
					} catch (Exception e) {
						// Catch exception for notifying the caller
						runException.set(e);
						// Do the standard behavior too -> logging
						throw new RuntimeException(e);
					} finally {
						doneSignal.countDown();
					}
				}
			});
		}

		// Wait for all Behaviors runnable to complete before continuing
		try {
			doneSignal.await();
		} catch (InterruptedException ex) {
			// This exception occurs when one of the launched task kills the agent before all the
			// submitted tasks are finished. Keep in mind that killing an agent should kill the
			// launched tasks.
			// Example of code that is generating this issue:
			//
			// on Initialize {
			//   in (100) [
			//     killMe
			//   ]
			// }
			//
			// In this example, the killMe is launched before the Initialize code is finished;
			// and because the Initialize event is fired through the current function, it
			// causes the InterruptedException.
		}

		// Re-throw the run-time exception
		if (runException.get() != null) {
			throw new ExecutionException(runException.get());
		}
	}

	/**
	 * Execute every single Behaviors runnable, a dedicated thread will created by the executor local to this class and be used to
	 * execute each runnable in parallel.
	 *
	 * <p>This function never fails. Errors in the event handlers are logged by the executor service.
	 *
	 * @param behaviorsMethodsToExecute the collection of Behaviors runnable that must be executed.
	 */
	private void executeAsynchronouslyBehaviorMethods(Collection<Runnable> behaviorsMethodsToExecute) {
		for (final Runnable runnable : behaviorsMethodsToExecute) {
			this.executor.execute(runnable);
		}
	}

}
