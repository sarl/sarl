/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.eventdispatching;

import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CopyOnWriteArraySet;

import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.reflect.TypeToken;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Event;

/**
 * Registry of all {@code BehaviorGuardEvaluator} classes containing a method to evaluate the guard of a given behavior (on clause in SARL behavior).
 * This class has been inspired by the {@code com.google.common.eventbus.SuscriberRegistry} class of Google Guava library.
 *
 * <p>This class is not thread-safe.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 *
 */
public class BehaviorGuardEvaluatorRegistry {

	/**
	 * Thread-safe cache of classes to their flattened hierarchy of supertypes.
	 */
	@SuppressWarnings("synthetic-access")
	private static final LoadingCache<Class<?>, ImmutableSet<Class<?>>> FLATTEN_HIERARCHY_CACHE = CacheBuilder.newBuilder().weakKeys()
			.build(new CacheLoader<Class<?>, ImmutableSet<Class<?>>>() {
				@Override
				public ImmutableSet<Class<?>> load(Class<?> concreteClass) {
					return getTypeHierarchyOnDemand(concreteClass);
				}
			});

	/**
	 * A thread-safe cache that contains the mapping from each class to all methods in that class and all super-classes, that are annotated with
	 * the annotation given by {@link #perceptGuardEvaluatorAnnotation}. The cache is shared across all instances of this class; this greatly
	 * improves performance if multiple EventBus instances are created and objects of the same class are registered on all of them.
	 */
	@SuppressWarnings("synthetic-access")
	private static final LoadingCache<Class<?>, Map<Class<? extends Event>, Collection<Method>>> PERCEPT_GUARD_EVALUATOR_METHOD_CACHE =
		CacheBuilder.newBuilder().weakKeys()
			.build(new CacheLoader<Class<?>, Map<Class<? extends Event>, Collection<Method>>>() {
				@Override
				public Map<Class<? extends Event>, Collection<Method>> load(Class<?> concreteClass) throws Exception {
					return getAnnotatedMethodMapOnDemand(concreteClass);
				}
			});

	/**
	 * All registered {@code BehaviorGuardEvaluator}s (class containing at least one PerceptGuardEvaluator method), indexed by event type.
	 *
	 * <p>
	 * The {@link CopyOnWriteArraySet} values make it easy and relatively lightweight to get an immutable snapshot of all current
	 * {@code BehaviorGuardEvaluator}s to an event without any locking.
	 * </p>
	 */
	private final Map<Class<? extends Event>,
		Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>>> behaviorGuardEvaluators;

	private boolean active = true;

	/**
	 * Instanciates a new registry linked with the {@link PerceptGuardEvaluator} annotation.
	 *
	 * <p>The registry will use concurrent data structures.
	 */
	public BehaviorGuardEvaluatorRegistry() {
		this(true);
	}

	/**
	 * Instanciates a new registry linked with the {@link PerceptGuardEvaluator} annotation.
	 *
	 * @param concurrent indicates if the internal data structures must support thread concurrency, or not.
	 */
	public BehaviorGuardEvaluatorRegistry(boolean concurrent) {
		this(concurrent ? Maps.newConcurrentMap() : Maps.newHashMap());
	}

	/**
	 * Instanciates a new registry linked with the {@link PerceptGuardEvaluator} annotation.
	 *
	 * @param buffer the buffer to be used for storing the behavior guard evaluators.
	 */
	public BehaviorGuardEvaluatorRegistry(Map<Class<? extends Event>, Pair<Function1<? super Event, ? extends Boolean>,
			Set<BehaviorGuardEvaluator>>> buffer) {
		assert buffer != null;
		this.behaviorGuardEvaluators = buffer;
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on the given listener object.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to add
	 * @param callback function which is invoked just after the first registration of the object. It could be {@code null}.
	 * @since 0.5
	 */
	public void register(Object listener, Procedure1<Object> callback) {
		register(listener, null, callback);
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on the given listener object.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to add
	 */
	public void register(Object listener) {
		register(listener, null, null);
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on the given listener object.
	 *
	 * <p>If the filter is provided, it will be used for determining if the given behavior accepts a specific event.
	 * If the filter function replies {@code true} for a specific event as argument, the event is fired in the
	 * behavior context. If the filter function replies {@code false}, the event is not fired in the behavior context.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to add
	 * @param filter the filter function.
	 * @since 0.5
	 */
	public void register(Object listener, Function1<? super Event, ? extends Boolean> filter) {
		register(listener, filter, null);
	}

	/**
	 * Registers all {@code PerceptGuardEvaluator} methods on the given listener object.
	 *
	 * <p>If the filter is provided, it will be used for determining if the given behavior accepts a specific event.
	 * If the filter function replies {@code true} for a specific event as argument, the event is fired in the
	 * behavior context. If the filter function replies {@code false}, the event is not fired in the behavior context.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to add
	 * @param filter the filter function.
	 * @param callback function which is invoked just after the first registration of the object. It could be {@code null}.
	 * @since 0.5
	 */
	public void register(Object listener, Function1<? super Event, ? extends Boolean> filter, Procedure1<Object> callback) {
		if (this.active) {
			boolean firstInit = false;
			final Iterator<Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>>> listenerMethods = new EvaluatorIterator(listener);
			while (listenerMethods.hasNext()) {
				final Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>> entry = listenerMethods.next();
				final Class<? extends Event> eventType = entry.getKey();
				final Collection<BehaviorGuardEvaluator> eventMethodsInListener = entry.getValue();

				final Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> pair =
						this.behaviorGuardEvaluators.get(eventType);
				final Set<BehaviorGuardEvaluator> eventSubscribers;

				if (pair == null) {
					//TODO Array-based implementation may be not efficient
					eventSubscribers = new CopyOnWriteArraySet<>();
					this.behaviorGuardEvaluators.put(eventType, new Pair<>(filter, eventSubscribers));
					firstInit = true;
				} else {
					eventSubscribers = pair.getValue();
				}

				if (eventSubscribers.addAll(eventMethodsInListener)) {
					firstInit = true;
				}
			}
			if (firstInit && callback != null) {
				callback.apply(listener);
			}
		}
	}

	/**
	 * Unregisters all BehaviorGuardEvaluators on all the listener objects.
	 */
	public void unregisterAll() {
		unregisterAll(null);
	}

	/**
	 * Unregisters all BehaviorGuardEvaluators on all the listener objects.
	 *
	 * @param callback function which is invoked just before the object is unregistered. It could be {@code null}.
	 * @since 0.5
	 */
	public void unregisterAll(Procedure1<Object> callback) {
		if (this.active) {
			this.active = false;
			try {
				if (callback != null) {
					final Iterator<Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>>> iterator =
							this.behaviorGuardEvaluators.values().iterator();
					final Set<Object> subscribers = Sets.newTreeSet((term1, term2) -> {
						if (term1 == term2) {
							return 0;
						}
						return Integer.compare(System.identityHashCode(term1), System.identityHashCode(term2));
					});
					while (iterator.hasNext()) {
						final Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> pair = iterator.next();
						for (final BehaviorGuardEvaluator evaluator : pair.getValue()) {
							if (subscribers.add(evaluator.getTarget())) {
								callback.apply(evaluator.getTarget());
							}
						}
						iterator.remove();
					}
				} else {
					this.behaviorGuardEvaluators.clear();
				}
			} finally {
				this.active = true;
			}
		}
	}

	/**
	 * Unregisters all BehaviorGuardEvaluators on the given listener object.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to remove
	 */
	public void unregister(Object listener) {
		unregister(listener, null);
	}

	/**
	 * Unregisters all BehaviorGuardEvaluators on the given listener object.
	 *
	 * @param listener the new {@code BehaviorGuardEvaluator} to remove
	 * @param callback function which is invoked just before the object is unregistered. It could be {@code null}.
	 * @since 0.5
	 */
	public void unregister(Object listener, Procedure1<Object> callback) {
		if (this.active) {
			final Iterator<Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>>> listenerMethods = new EvaluatorIterator(listener);
			Procedure1<Object> listenerCallback = callback;
			while (listenerMethods.hasNext()) {
				final Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>> entry = listenerMethods.next();
				final Class<? extends Event> eventType = entry.getKey();
				final Collection<BehaviorGuardEvaluator> listenerMethodsForType = entry.getValue();

				final Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> pair =
						this.behaviorGuardEvaluators.get(eventType);

				if (listenerCallback != null && pair != null && pair.getValue() != null && !pair.getValue().isEmpty()) {
					listenerCallback.apply(listener);
					listenerCallback = null;
				}

				if (pair == null || pair.getValue() == null || !pair.getValue().removeAll(listenerMethodsForType)) {
					if (pair != null && pair.getValue() != null) {
						pair.getValue().removeAll(listenerMethodsForType);
					}
					// if removeAll returns true, all we really know is that at least one subscriber was
					// removed... however, barring something very strange we can assume that if at least one
					// subscriber was removed, all subscribers on listener for that event type were... after
					// all, the definition of subscribers on a particular class is totally static
					throw new IllegalArgumentException(MessageFormat.format(Messages.BehaviorGuardEvaluatorRegistry_0, listener));
				}

				// don't try to remove the set if it's empty; that can't be done safely without a lock
				// anyway, if the set is empty it'll just be wrapping an array of length 0
			}
		}
	}

	/**
	 * Gets an iterator representing an immutable snapshot of all BehaviorGuardEvaluators to the given event at the time this method is called.
	 *
	 * @param event
	 *            -the event to process
	 * @return the set of guard evaluators associated to the specified event
	 */
	public Iterable<BehaviorGuardEvaluator> getBehaviorGuardEvaluators(Event event) {
		//		final ImmutableSet<Class<?>> eventTypes = flattenHierarchy(event.getClass());
		//
		//		final List<BehaviorGuardEvaluator> iBehaviorGuardEvaluators = Lists.newArrayListWithCapacity(eventTypes.size());
		//
		//		for (final Class<?> eventType : eventTypes) {
		//			final Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> eventSubscribers =
		//					this.behaviorGuardEvaluators.get(eventType);
		//			if (eventSubscribers != null && eventSubscribers.getValue() != null
		//					&& (eventSubscribers.getKey() == null || eventSubscribers.getKey().apply(event))) {
		//				iBehaviorGuardEvaluators.addAll(eventSubscribers.getValue());
		//			}
		//		}
		//
		//		return iBehaviorGuardEvaluators;
		final Iterable<Class<?>> eventTypes = flattenHierarchy(event.getClass());
		return () -> {
			return new EvaluatorCollectionIterator(this.behaviorGuardEvaluators, event,
					eventTypes.iterator(), false);
		};
	}

	/**
	 * Gets an iterator representing an immutable snapshot of all BehaviorGuardEvaluators of the given listener
	 * to the given event at the time this method is called.
	 *
	 * <p>Caution: This function does not apply filtering function given to {@link #register(Object, Function1, Procedure1)}.
	 *
	 * @param event -the event to process
	 * @param listener - the owner of the BehaviorGuardEvaluators (never {@code null}).
	 * @return the set of guard evaluators associated to the specified event
	 * @since 0.5
	 */
	public Iterable<BehaviorGuardEvaluator> getBehaviorGuardEvaluatorsFor(Event event, Object listener) {
		final Iterable<Class<?>> eventTypes = flattenHierarchy(event.getClass());
		return () -> {
			final Iterator<BehaviorGuardEvaluator> base = new EvaluatorCollectionIterator(this.behaviorGuardEvaluators, event,
					eventTypes.iterator(), true);
			return new EvaluatorCollectionFilteringIterator(base, listener);
		};
	}

	/** Replies if a listener with the given type is registered.
	 *
	 * @param type the type of listener.
	 * @return {@code true} if a listener of the given type is registered.
	 * @since 0.5
	 */
	public boolean hasRegisteredEventListener(Class<?> type) {
		// TODO: Not efficient implementation
		for (final Pair<?, Set<BehaviorGuardEvaluator>> pair : this.behaviorGuardEvaluators.values()) {
			final Set<BehaviorGuardEvaluator> listeners = pair.getValue();
			if (listeners != null) {
				for (final BehaviorGuardEvaluator evaluator : listeners) {
					final Object target = evaluator.getTarget();
					if (type.isInstance(target)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/** Extract the registered listeners with the given type.
	 *
	 * @param <T> the type of the listeners.
	 * @param type the type of the listeners.
	 * @param collection the collection of listeners that is filled by this function. This argument could be {@code null}.
	 * @return the number of listeners of the given type.
	 * @since 0.5
	 */
	public <T> int getRegisteredEventListeners(Class<T> type, Collection<? super T> collection) {
		// TODO: Not efficient implementation
		final Set<Object> addedObjects = new TreeSet<>((elt1, elt2) -> {
			if (elt1 == elt2) {
				return 0;
			}
			return Integer.compare(System.identityHashCode(elt1), System.identityHashCode(elt2));
		});
		for (final Pair<?, Set<BehaviorGuardEvaluator>> pair : this.behaviorGuardEvaluators.values()) {
			final Set<BehaviorGuardEvaluator> listeners = pair.getValue();
			if (listeners != null) {
				for (final BehaviorGuardEvaluator evaluator : listeners) {
					final Object target = evaluator.getTarget();
					if (type.isInstance(target) && addedObjects.add(target)) {
						if (collection != null) {
							collection.add(type.cast(target));
						}
					}
				}
			}
		}
		return addedObjects.size();
	}

	private static Map<Class<? extends Event>, Collection<Method>> getAnnotatedMethodsPerEvent(Class<?> listenerType) {
		try {
			return PERCEPT_GUARD_EVALUATOR_METHOD_CACHE.getUnchecked(listenerType);
		} catch (Exception ex) {
			throw Throwables.propagate(ex);
		}
	}

	private static Class<?> reloadClass(Class<?> context, Class<?> type) {
		ClassLoader ld = context.getClassLoader();
		if (ld == null) {
			ld = type.getClassLoader();
		}
		try {
			return ld.loadClass(type.getName());
		} catch (Throwable ex) {
			return type;
		}
	}

	private static boolean checkEventHandlerPrototype(Class<?>[] parameterTypes) {
		try {
			if (parameterTypes.length == 2
				&& parameterTypes[0] != null
				&& reloadClass(parameterTypes[0], Event.class).isAssignableFrom(parameterTypes[0])
				&& parameterTypes[1] != null
				&& reloadClass(parameterTypes[1], Collection.class).isAssignableFrom(parameterTypes[1])) {
				return true;
			}
		} catch (Exception ex) {
			//
		}
		return false;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static ImmutableSet<Class<?>> getTypeHierarchyOnDemand(Class<?> concreteClass) {
		assert concreteClass != null;
		final Set typeHierarchy = TypeToken.of(concreteClass).getTypes().rawTypes();
		try {
			final Class<?> eventType = reloadClass(concreteClass, Event.class);
			if (eventType.isAssignableFrom(concreteClass)) {
				return ImmutableSet.copyOf(Sets.filter((Set<Class>) typeHierarchy,
						(it) -> eventType.isAssignableFrom(it)));
			}
		} catch (Exception ex) {
			//
		}
		return ImmutableSet.copyOf(typeHierarchy);
	}

	@SuppressWarnings("unchecked")
	private static Map<Class<? extends Event>, Collection<Method>> getAnnotatedMethodMapOnDemand(Class<?> concreteClass) {
		assert concreteClass != null;
		// TODO verify it effectively explores the whole type hierarchy
		final Set<? extends Class<?>> supertypes = Sets.filter(TypeToken.of(concreteClass).getTypes().rawTypes(),
				(it) -> !it.isInterface() && !Object.class.equals(it));

		final Map<MethodIdentifier, Method> identifiers = Maps.newTreeMap();

		// Traverse all methods of the whole inheritance hierarchy
		for (final Class<?> supertype : supertypes) {
			for (final Method method : supertype.getDeclaredMethods()) {
				if (method.isAnnotationPresent(PerceptGuardEvaluator.class) && !method.isSynthetic()) {
					final Class<?>[] parameterTypes = method.getParameterTypes();
					final MethodIdentifier ident = new MethodIdentifier(method, parameterTypes);
					identifiers.putIfAbsent(ident, method);
				}
			}
		}

		final Map<Class<? extends Event>, Collection<Method>> buffer = Maps.newTreeMap(
				(elt1, elt2) -> elt1.getName().compareTo(elt2.getName()));
		for (final Method method : identifiers.values()) {
			final Class<?>[] parameterTypes = method.getParameterTypes();
			// Check the prototype of the event handler in debug mode only
			assert checkEventHandlerPrototype(parameterTypes);
			final Class<? extends Event> eventType = (Class<? extends Event>) parameterTypes[0];
			Collection<Method> methods = buffer.get(eventType);
			if (methods == null) {
				methods = Lists.newArrayList();
				buffer.put(eventType, methods);
			}
			methods.add(method);
		}

		return ImmutableMap.copyOf(buffer);
	}

	/**
	 * Flattens a class's type hierarchy into a set of {@code Class} objects including all super-classes (transitively) and all interfaces implemented
	 * by these super-classes.
	 *
	 * @param concreteClass
	 *            - the class you find the hierarchy
	 * @return the set of class in the hierarchy of the specififed class
	 */
	private static ImmutableSet<Class<?>> flattenHierarchy(Class<?> concreteClass) {
		try {
			return FLATTEN_HIERARCHY_CACHE.getUnchecked(concreteClass);
		} catch (Exception e) {
			throw Throwables.propagate(e.getCause());
		}
	}

	/**
	 * It stores the information related to a given method especially its prototype.
	 *
	 * @author $Author: ngaud$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 *
	 */
	private static final class MethodIdentifier implements Comparable<MethodIdentifier> {

		/**
		 * the name of the considered method.
		 */
		private final String name;

		/**
		 * The list of the type of the various parameters of the considered method.
		 */
		private final List<Class<?>> parameterTypes;

		/**
		 * Creates a new method identifier according to the name and the list of parameter types of the considered method.
		 *
		 * @param method
		 *            - the name of the considered method.
		 * @param parameterTypes
		 *            - The list of the type of the various parameters of the considered method.
		 */
		MethodIdentifier(Method method, Class<?>[] parameterTypes) {
			// Useful to maintain the getDeclaringClass to obtain method of all classes with the inheritance hierarchy
			this.name = method.getDeclaringClass() + method.getName();
			this.parameterTypes = Arrays.asList(parameterTypes);
		}

		@Override
		public int hashCode() {
			return Objects.hashCode(this.name, this.parameterTypes);
		}

		@Override
		public boolean equals(Object object) {
			if (object instanceof MethodIdentifier) {
				final MethodIdentifier ident = (MethodIdentifier) object;
				return this.name.equals(ident.name) && this.parameterTypes.equals(ident.parameterTypes);
			}
			return false;
		}

		@Override
		public int compareTo(MethodIdentifier obj) {
			if (obj == null) {
				return -1;
			}
			int cmp = this.name.compareTo(obj.name);
			if (cmp != 0) {
				return cmp;
			}
			cmp = Integer.compare(this.parameterTypes.size(), obj.parameterTypes.size());
			if (cmp != 0) {
				return cmp;
			}
			final Iterator<Class<?>> it1 = this.parameterTypes.iterator();
			final Iterator<Class<?>> it2 = obj.parameterTypes.iterator();
			while (it1.hasNext()) {
				assert it2.hasNext();
				cmp = it1.next().getName().compareTo(it2.next().getName());
				if (cmp != 0) {
					return cmp;
				}
			}
			return 0;
		}

		@Override
		public String toString() {
			return this.name;
		}

	}

	/** Iterator on guard evaluators.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class EvaluatorIterator implements Iterator<Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>>> {

		private final Object listener;

		private final Iterator<Entry<Class<? extends Event>, Collection<Method>>> iterator;

		@SuppressWarnings("synthetic-access")
		EvaluatorIterator(Object listener) {
			final Map<Class<? extends Event>, Collection<Method>> methods = getAnnotatedMethodsPerEvent(listener.getClass());
			this.iterator = methods.entrySet().iterator();
			this.listener = listener;
		}

		@Override
		public boolean hasNext() {
			return this.iterator.hasNext();
		}

		@Override
		public Pair<Class<? extends Event>, Collection<BehaviorGuardEvaluator>> next() {
			final Entry<Class<? extends Event>, Collection<Method>> entry = this.iterator.next();
			return new Pair<>(entry.getKey(), Collections2.transform(entry.getValue(),
					(element) -> new BehaviorGuardEvaluator(this.listener, element)));
		}

	}

	/** Iterator on behavior guard evaluators.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	private static class EvaluatorCollectionIterator implements Iterator<BehaviorGuardEvaluator> {

		private final Map<Class<? extends Event>,
			Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>>> behaviorGuardEvaluators;

		private final Event event;

		private final Iterator<Class<?>> eventTypeIterator;

		private final boolean skipSubscriberFiltering;

		private Iterator<BehaviorGuardEvaluator> evaluators;

		EvaluatorCollectionIterator(Map<Class<? extends Event>,
				Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>>> behaviorGuardEvaluators,
				Event event,
				Iterator<Class<?>> eventTypes,
				boolean skipSubscriberFiltering) {
			assert behaviorGuardEvaluators != null;
			assert event != null;
			this.skipSubscriberFiltering = skipSubscriberFiltering;
			this.behaviorGuardEvaluators = behaviorGuardEvaluators;
			this.event = event;
			this.eventTypeIterator = eventTypes;
			searchNext();
		}

		private void searchNext() {
			while ((this.evaluators == null || !this.evaluators.hasNext()) && this.eventTypeIterator.hasNext()) {
				final Class<?> eventType = this.eventTypeIterator.next();
				final Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> eventSubscribers =
						this.behaviorGuardEvaluators.get(eventType);
				if (isValidSubscriber(eventSubscribers)) {
					this.evaluators = eventSubscribers.getValue().iterator();
				}
			}
		}

		protected boolean isValidSubscriber(Pair<Function1<? super Event, ? extends Boolean>, Set<BehaviorGuardEvaluator>> subscriber) {
			if (subscriber != null && subscriber.getValue() != null) {
				return this.skipSubscriberFiltering || subscriber.getKey() == null || subscriber.getKey().apply(this.event);
			}
			return false;
		}

		@Override
		public boolean hasNext() {
			return this.evaluators != null && this.evaluators.hasNext();
		}

		@Override
		public BehaviorGuardEvaluator next() {
			if (this.evaluators == null) {
				searchNext();
			}
			final BehaviorGuardEvaluator next = this.evaluators.next();
			searchNext();
			return next;
		}

	}

	/** Iterator on behavior guard evaluators.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	private static class EvaluatorCollectionFilteringIterator implements Iterator<BehaviorGuardEvaluator> {

		private final Iterator<BehaviorGuardEvaluator> iterator;

		private final Object subscriber;

		private BehaviorGuardEvaluator next;

		EvaluatorCollectionFilteringIterator(Iterator<BehaviorGuardEvaluator> iterator, Object subscriber) {
			assert iterator != null;
			assert subscriber != null;
			this.iterator = iterator;
			this.subscriber = subscriber;
			searchNext();
		}

		private void searchNext() {
			this.next = null;
			while (this.next == null && this.iterator.hasNext()) {
				final BehaviorGuardEvaluator evaluator = this.iterator.next();
				if (evaluator.getTarget() == this.subscriber) {
					this.next = evaluator;
				}
			}
		}

		@Override
		public boolean hasNext() {
			return this.next != null;
		}

		@Override
		public BehaviorGuardEvaluator next() {
			assert this.next != null;
			final BehaviorGuardEvaluator next = this.next;
			searchNext();
			return next;
		}

	}

}
