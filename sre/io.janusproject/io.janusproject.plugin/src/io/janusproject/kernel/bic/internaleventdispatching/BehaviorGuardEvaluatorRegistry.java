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

import static com.google.common.base.Preconditions.checkArgument;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArraySet;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.reflect.TypeToken;
import com.google.common.util.concurrent.UncheckedExecutionException;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Event;

/**
 * Registry of all {@code BehaviorGuardEvaluator} classes containing a method to evaluate the guard of a given behavior (on clause in SARL behavior).
 * This class has been inspired by the com.google.common.eventbus.SuscriberRegistry class of Google Guava library.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 *
 */
public class BehaviorGuardEvaluatorRegistry {

    /**
     * Global cache of classes to their flattened hierarchy of supertypes.
     */
    private static final LoadingCache<Class<?>, ImmutableSet<Class<?>>> FLATTEN_HIERARCHY_CACHE = CacheBuilder.newBuilder().weakKeys()
            .build(new CacheLoader<Class<?>, ImmutableSet<Class<?>>>() {
                @Override
                public ImmutableSet<Class<?>> load(Class<?> concreteClass) {
                    return ImmutableSet.<Class<?>>copyOf(TypeToken.of(concreteClass).getTypes().rawTypes());
                }
            });

    /**
     * A thread-safe cache that contains the mapping from each class to all methods in that class and all super-classes, that are annotated with
     * {@code @Subscribe}. The cache is shared across all instances of this class; this greatly improves performance if multiple EventBus instances
     * are created and objects of the same class are registered on all of them.
     */
    @SuppressWarnings("synthetic-access")
    private final LoadingCache<Class<?>, ImmutableList<Method>> perceptGuardEvaluatorMethodsCache = CacheBuilder.newBuilder().weakKeys()
            .build(new CacheLoader<Class<?>, ImmutableList<Method>>() {
                @Override
                public ImmutableList<Method> load(Class<?> concreteClass) throws Exception {
                    return getAnnotatedMethodsNotCached(concreteClass);
                }
            });

    /**
     * The annotation used to identify methods considered as the evaluator of the guard of a given behavior (on clause in SARL behavior) If class has
     * a such method, it is considered as a {@code BehaviorGuardEvaluator}.
     */
    private Class<? extends Annotation> perceptGuardEvaluatorAnnotation;

    /**
     * All registered {@code BehaviorGuardEvaluator}s (class containing at least one PerceptGuardEvaluator method), indexed by event type.
     *
     * <p>
     * The {@link CopyOnWriteArraySet} values make it easy and relatively lightweight to get an immutable snapshot of all current
     * {@code BehaviorGuardEvaluator}s to an event without any locking.
     * </p>
     */
    private final ConcurrentMap<Class<? extends Event>, CopyOnWriteArraySet<BehaviorGuardEvaluator>> behaviorGuardEvaluators = Maps
            .newConcurrentMap();

    /**
     * Instanciates a new registry.
     *
     * @param annotation
     *            - The annotation used to identify methods considered as the evaluator of the guard of a given behavior (on clause in SARL behavior)
     *            If class has a such method, it is considered as a {@code BehaviorGuardEvaluator}.
     */
    BehaviorGuardEvaluatorRegistry(Class<? extends Annotation> annotation) {

        this.perceptGuardEvaluatorAnnotation = annotation;
    }

    /**
     * Registers all {@code PerceptGuardEvaluator} methods on the given listener object.
     *
     * @param listener
     *            - the new {@code BehaviorGuardEvaluator} to add
     */
    void register(Object listener) {
        final Multimap<Class<? extends Event>, BehaviorGuardEvaluator> listenerMethods = findAllBehaviorGuardEvaluators(listener);

        for (final Map.Entry<Class<? extends Event>, Collection<BehaviorGuardEvaluator>> entry : listenerMethods.asMap().entrySet()) {
            final Class<? extends Event> eventType = entry.getKey();
            final Collection<BehaviorGuardEvaluator> eventMethodsInListener = entry.getValue();

            CopyOnWriteArraySet<BehaviorGuardEvaluator> eventSubscribers = this.behaviorGuardEvaluators.get(eventType);

            if (eventSubscribers == null) {
                final CopyOnWriteArraySet<BehaviorGuardEvaluator> newSet = new CopyOnWriteArraySet<>();
                eventSubscribers = MoreObjects.firstNonNull(this.behaviorGuardEvaluators.putIfAbsent(eventType, newSet), newSet);
            }

            eventSubscribers.addAll(eventMethodsInListener);
        }
    }

    /**
     * Unregisters all BehaviorGuardEvaluators on the given listener object.
     *
     * @param listener
     *            - the new {@code BehaviorGuardEvaluator} to add
     */
    void unregister(Object listener) {
        final Multimap<Class<? extends Event>, BehaviorGuardEvaluator> listenerMethods = findAllBehaviorGuardEvaluators(listener);

        for (final Map.Entry<Class<? extends Event>, Collection<BehaviorGuardEvaluator>> entry : listenerMethods.asMap().entrySet()) {
            final Class<? extends Event> eventType = entry.getKey();
            final Collection<BehaviorGuardEvaluator> listenerMethodsForType = entry.getValue();

            final CopyOnWriteArraySet<BehaviorGuardEvaluator> currentSubscribers = this.behaviorGuardEvaluators.get(eventType);
            if (currentSubscribers == null || !currentSubscribers.removeAll(listenerMethodsForType)) {

                if (currentSubscribers != null) {
                    currentSubscribers.removeAll(listenerMethodsForType);
                }
                // if removeAll returns true, all we really know is that at least one subscriber was
                // removed... however, barring something very strange we can assume that if at least one
                // subscriber was removed, all subscribers on listener for that event type were... after
                // all, the definition of subscribers on a particular class is totally static
                throw new IllegalArgumentException(Locale.getString(getClass(), "ANNOTATION_MISSED", listener)); //$NON-NLS-1$
            }

            // don't try to remove the set if it's empty; that can't be done safely without a lock
            // anyway, if the set is empty it'll just be wrapping an array of length 0
        }
    }

    /**
     * Gets an iterator representing an immutable snapshot of all BehaviorGuardEvaluators to the given event at the time this method is called.
     *
     * @param event
     *            -the event to process
     * @return the set of guard evaluators associated to the specified event
     */
    Collection<BehaviorGuardEvaluator> getBehaviorGuardEvaluators(Object event) {
        final ImmutableSet<Class<?>> eventTypes = flattenHierarchy(event.getClass());

        final List<BehaviorGuardEvaluator> iBehaviorGuardEvaluators = Lists.newArrayListWithCapacity(eventTypes.size());

        for (final Class<?> eventType : eventTypes) {
            final CopyOnWriteArraySet<BehaviorGuardEvaluator> eventSubscribers = this.behaviorGuardEvaluators.get(eventType);
            if (eventSubscribers != null) {
                iBehaviorGuardEvaluators.addAll(eventSubscribers);
            }
        }

        return iBehaviorGuardEvaluators;
    }

    /**
     * Returns all {@code BehaviorGuardEvaluator}s for the given listener grouped by the type of event they subscribe to.
     *
     * @param listener
     *            - the listener
     * @return a map associating event classes to their guard evaluators
     */
    @SuppressWarnings("unchecked")
    private Multimap<Class<? extends Event>, BehaviorGuardEvaluator> findAllBehaviorGuardEvaluators(Object listener) {
        final Multimap<Class<? extends Event>, BehaviorGuardEvaluator> methodsInListener = HashMultimap.create();
        final Class<?> clazz = listener.getClass();
        for (final Method method : getAnnotatedMethods(clazz)) {
            final Class<?>[] parameterTypes = method.getParameterTypes();
            final Class<? extends Event> eventType = (Class<? extends Event>) parameterTypes[0];
            methodsInListener.put(eventType, BehaviorGuardEvaluator.create(listener, method));
        }
        return methodsInListener;
    }

    private ImmutableList<Method> getAnnotatedMethods(Class<?> clazz) {
        return this.perceptGuardEvaluatorMethodsCache.getUnchecked(clazz);
    }

    private ImmutableList<Method> getAnnotatedMethodsNotCached(Class<?> clazz) {
        // TODO verify it effectively explores the whole type hierarchy
        final Set<? extends Class<?>> supertypes = TypeToken.of(clazz).getTypes().rawTypes();

        final Map<MethodIdentifier, Method> identifiers = Maps.newHashMap();

        for (final Class<?> supertype : supertypes) {
            // Traverse all methods of the whole inheritance hierarchy
            for (final Method method : supertype.getDeclaredMethods()) {
                if (method.isAnnotationPresent(this.perceptGuardEvaluatorAnnotation) && !method.isSynthetic()) {
                    // FIXME: Should check for a generic parameter type and error out
                    final Class<?>[] parameterTypes = method.getParameterTypes();
                    checkArgument(parameterTypes.length == 2,
                    		Locale.getString(getClass(), "INVALID_PROTOTYPE", //$NON-NLS-1$
                    				method, this.perceptGuardEvaluatorAnnotation.toString(),
                    				Integer.valueOf(parameterTypes.length)));

                    final MethodIdentifier ident = new MethodIdentifier(method, parameterTypes);
                    identifiers.put(ident, method);
                }
            }
        }

        return ImmutableList.copyOf(identifiers.values());
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
        } catch (UncheckedExecutionException e) {
            throw Throwables.propagate(e.getCause());
        }
    }

    /**
     * It stores the information related to a given method especially its prototype.
     *
     * @author $Author: ngaud$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     *
     */
    private static final class MethodIdentifier {

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

        /**
         * {@inheritDoc}.
         */
        @Override
        public int hashCode() {
            return Objects.hashCode(this.name, this.parameterTypes);
        }

        /**
         * {@inheritDoc}.
         */
        @Override
        public boolean equals(Object object) {
            if (object instanceof MethodIdentifier) {
                final MethodIdentifier ident = (MethodIdentifier) object;
                return this.name.equals(ident.name) && this.parameterTypes.equals(ident.parameterTypes);
            }
            return false;
        }
    }

}
