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

package io.janusproject.kernel.bic.internaleventdispatching;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Queue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;

import com.google.common.collect.Lists;
import com.google.common.collect.Queues;
import io.janusproject.services.executor.ExecutorService;
import org.arakhne.afc.util.MultiCollection;
import org.arakhne.afc.util.OutputParameter;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.core.DeadEvent;
import io.sarl.lang.core.Event;

/**
 * The class in charge of dispatching every single events coming from the outside of this agent (i.e. from a space) or from an
 * agent's behavior.
 *
 * @author $Author: ngaud$
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
	 * Per-thread queue of events to dispatch.
	 */
	private final ThreadLocal<Queue<Pair<Event, Collection<Runnable>>>> queue = new ThreadLocal<Queue<Pair<Event, Collection<Runnable>>>>() {
		@Override
		protected Queue<Pair<Event, Collection<Runnable>>> initialValue() {
			return Queues.newArrayDeque();
		}
	};

	/**
	 * Per-thread dispatch state, used to avoid reentrant event dispatching.
	 */
	private final ThreadLocal<Boolean> dispatching = new ThreadLocal<Boolean>() {
		@Override
		protected Boolean initialValue() {
			return Boolean.FALSE;
		}
	};

	/**
	 * The executor used to execute behavior methods in dedicated thread.
	 */
	@Inject
	private ExecutorService executor;

	/**
	 * Instantiates a dispatcher.
	 *
	 * @param perceptGuardEvaluatorAnnotation - The annotation used to identify methods considered as the evaluator of the guard
	 *        of a given behavior (on clause in SARL behavior) If class has a such method, it is considered as a
	 *        {@code BehaviorGuardEvaluator}.
	 */
	public AgentInternalEventsDispatcher(Class<? extends Annotation> perceptGuardEvaluatorAnnotation) {
		this.behaviorGuardEvaluatorRegistry = new BehaviorGuardEvaluatorRegistry(perceptGuardEvaluatorAnnotation);
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on {@code object} to receive events.
	 *
	 * @param object object whose {@code PerceptGuardEvaluator} methods should be registered.
	 */
	public void register(Object object) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			this.behaviorGuardEvaluatorRegistry.register(object);
		}
	}

	/**
	 * Unregisters all {@code PerceptGuardEvaluator} methods on a registered {@code object}.
	 *
	 * @param object object whose {@code PerceptGuardEvaluator} methods should be unregistered.
	 * @throws IllegalArgumentException if the object was not previously registered.
	 */
	public void unregister(Object object) {
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			this.behaviorGuardEvaluatorRegistry.unregister(object);
		}
	}

	/**
	 * Posts an event to all registered {@code BehaviorGuardEvaluator}, the dispatch of this event will be done synchronously.
	 * This method will return successfully after the event has been posted to all {@code BehaviorGuardEvaluator}, and regardless
	 * of any exceptions thrown by {@code BehaviorGuardEvaluator}.
	 *
	 * <p>
	 * If no {@code BehaviorGuardEvaluator} have been subscribed for {@code event}'s class, and {@code event} is not already a
	 * {@link DeadEvent}, it will be wrapped in a DeadEvent and reposted.
	 * </p>
	 *
	 * @param event - an event to dispatch synchronously.
	 */
	public void immediateDispatch(Event event) {
		assert event != null;
		Collection<BehaviorGuardEvaluator> behaviorGuardEvaluators = null;
		synchronized (this.behaviorGuardEvaluatorRegistry) {
			behaviorGuardEvaluators = AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry
					.getBehaviorGuardEvaluators(event);
		}
		if (behaviorGuardEvaluators != null && !behaviorGuardEvaluators.isEmpty()) {
			final Collection<Runnable> behaviorsMethodsToExecute;
			try {
				behaviorsMethodsToExecute = evaluateGuards(event, behaviorGuardEvaluators);
				executeBehaviorMethodsInParalellWithSynchroAtTheEnd(event, behaviorsMethodsToExecute);
			} catch (RuntimeException exception) {
				throw exception;
			} catch (InterruptedException | ExecutionException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}

		} else if (!(event instanceof DeadEvent)) {
			// the event had no subscribers and was not itself a DeadEvent
			immediateDispatch(new DeadEvent(event));
		}

	}

	/**
	 * Posts an event to all registered {@code BehaviorGuardEvaluator}, the dispatch of this event will be done asynchronously.
	 * This method will return successfully after the event has been posted to all {@code BehaviorGuardEvaluator}, and regardless
	 * of any exceptions thrown by {@code BehaviorGuardEvaluator}.
	 *
	 * <p>
	 * If no {@code BehaviorGuardEvaluator} have been subscribed for {@code event}'s class, and {@code event} is not already a
	 * {@link DeadEvent}, it will be wrapped in a DeadEvent and reposted.
	 * </p>
	 * @param event - an event to dispatch asynchronously.
	 */
	public void asyncDispatch(Event event) {
		assert event != null;
		this.executor.execute(() -> {
			Collection<BehaviorGuardEvaluator> behaviorGuardEvaluators = null;
			synchronized (AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry) {
				behaviorGuardEvaluators = AgentInternalEventsDispatcher.this.behaviorGuardEvaluatorRegistry
						.getBehaviorGuardEvaluators(event);
			}
			if (behaviorGuardEvaluators != null && !behaviorGuardEvaluators.isEmpty()) {

				final Collection<Runnable> behaviorsMethodsToExecute;
				try {
					behaviorsMethodsToExecute = evaluateGuards(event, behaviorGuardEvaluators);
				} catch (InvocationTargetException e) {
					throw new RuntimeException(e);
				}
				executeAsynchronouslyBehaviorMethods(event, behaviorsMethodsToExecute);

			} else if (!(event instanceof DeadEvent)) {
				// the event had no subscribers and was not itself a DeadEvent
				asyncDispatch(new DeadEvent(event));
			}
		});
	}

	/**
	 * Evaluate the guard associated to the specified {@code event} and returns the list of behaviors methods that must be
	 * executed.
	 *
	 * @param event - the event triggering behaviors
	 * @param behaviorGuardEvaluators - the list of class containing a {@code PerceptGuardEvaluator} method
	 * @return the collection of couple associating a object and its collection of behavior methods that must be executed
	 * @throws InvocationTargetException - exception when you try to execute a method by reflection and this method doesn't exist.
	 */
	private static Collection<Runnable> evaluateGuards(final Event event,
			final Collection<BehaviorGuardEvaluator> behaviorGuardEvaluators) throws InvocationTargetException {

		final MultiCollection<Runnable> behaviorsMethodsToExecute = new MultiCollection<>();

		try {
			behaviorGuardEvaluators.parallelStream().forEach((evaluator) -> {
				try {
					final Collection<Runnable> behaviorsMethodsToExecutePerTarget = Lists.newLinkedList();
					evaluator.evaluateGuard(event, behaviorsMethodsToExecutePerTarget);
					synchronized (behaviorsMethodsToExecute) {
						behaviorsMethodsToExecute.addCollection(behaviorsMethodsToExecutePerTarget);
					}
				} catch (InvocationTargetException exception) {
					throw new RuntimeException(exception);
				}
			});
		} catch (RuntimeException exception) {
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
	 * @param event - the event occurrence that has activated the specified behaviors, used just for indexing purpose but not
	 *        passed to runnable here, they were created according to this occurrence
	 * @param behaviorsMethodsToExecute - the collection of Behaviors runnable that must be executed.
	 * @throws InterruptedException - something interrupt the waiting of the event handler terminations.
	 * @throws ExecutionException - when the event handlers cannot be called; or when one of the event handler has failed during
	 *     its run.
	 */
	private void executeBehaviorMethodsInParalellWithSynchroAtTheEnd(Event event, Collection<Runnable> behaviorsMethodsToExecute)
			throws InterruptedException, ExecutionException {

		final CountDownLatch doneSignal = new CountDownLatch(behaviorsMethodsToExecute.size());

		final OutputParameter<Throwable> runException = new OutputParameter<>();

		for (final Runnable runnable : behaviorsMethodsToExecute) {
			this.executor.execute(() -> {
				try {
					runnable.run();
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
			});
		}

		// Wait for all Behaviors runnable to complete before continuing
		doneSignal.await();

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
	 * @param event - the event occurrence that has activated the specified behaviors, used just for indexing purpose but not
	 *        passed to runnable here, they were created according to this occurrence
	 * @param behaviorsMethodsToExecute - the collection of Behaviors runnable that must be executed.
	 */
	private void executeAsynchronouslyBehaviorMethods(Event event, Collection<Runnable> behaviorsMethodsToExecute) {

		final Queue<Pair<Event, Collection<Runnable>>> queueForThread = this.queue.get();
		queueForThread.offer(new Pair<>(event, behaviorsMethodsToExecute));

		if (!this.dispatching.get().booleanValue()) {
			this.dispatching.set(Boolean.TRUE);
			try {
				Pair<Event, Collection<Runnable>> nextEvent;
				while ((nextEvent = queueForThread.poll()) != null) {
					for (final Runnable runnable : nextEvent.getValue()) {
						this.executor.execute(runnable);
					}
				}
			} finally {
				this.dispatching.remove();
				this.queue.remove();
			}
		}

	}

}
