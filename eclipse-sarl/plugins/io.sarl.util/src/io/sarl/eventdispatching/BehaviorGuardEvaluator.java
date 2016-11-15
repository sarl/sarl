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

package io.sarl.eventdispatching;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Collection;

/**
 * Describes each class having one of its methods annotated with {@code PerceptGuardEvaluator} annotation corresponding to the
 * method in charge of evaluating the guard associated to a given event and returns the list of behaviors runnable that must be
 * executed according to the result of the guard evaluation.
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 *
 */
public final class BehaviorGuardEvaluator {

    /** The object with the {@code PerceptGuardEvaluator} method. */
    private final Object target;

    /** {@code PerceptGuardEvaluator} method. */
    private final Method method;

    private BehaviorGuardEvaluator(Object target, Method method) {
    	assert target != null;
        this.target = target;
        this.method = method;
    }

	/**
	 * Creates a {@code Subscriber} for {@code method} on {@code listener}.
	 * @param listener - the listener
	 * @param method - the method to call to evaluate a guard
	 * @return a BehaviorGuardEvaluator
	 */
	static BehaviorGuardEvaluator create(Object listener, Method method) {
		return new BehaviorGuardEvaluator(listener, method);
	}

	/**
	 * Evaluates the guard associated to the specified {@code event} and returns the list of behaviors methods that must be
	 * executed.
	 * @param event - the event triggering behaviors
	 * @param behaviorsMethodsToExecute - the list of behavior methods that will be completed according to the result of the guard
	 *        evaluation, BE CARFEUL: we suppose that these behavior methods are parts of the SAME object where the
	 *        {@code PerceptGuardEvaluator} method is declared
	 * @throws InvocationTargetException - exception during evaluation, can find the method to invoke
	 */
	public void evaluateGuard(final Object event, Collection<Runnable> behaviorsMethodsToExecute)
			throws InvocationTargetException {
		invokeBehaviorGuardEvaluatorMethod(event, behaviorsMethodsToExecute);
	}

	/**
	 * Invokes the subscriber method. This method can be overridden to make the invocation synchronized.
     * @param event - the event triggering behaviors
     * @param behaviorsMethodsToExecute - the list of behavior methods that will be completed according to the result of the guard
     *        evaluation, BE CARFEUL: we suppose that these behavior methods are parts of the SAME object where the
     *        {@code PerceptGuardEvaluator} method is declared
     * @throws InvocationTargetException - exception during evaluation, can find the method to invoke
	 */
	private void invokeBehaviorGuardEvaluatorMethod(Object event, Collection<Runnable> behaviorsMethodsToExecute)
			throws InvocationTargetException {
		try {
			this.method.setAccessible(true);
			this.method.invoke(this.target, event, behaviorsMethodsToExecute);
		} catch (IllegalArgumentException e) {
			throw new Error(MessageFormat.format(Messages.BehaviorGuardEvaluator_0, event), e);
		} catch (IllegalAccessException e) {
			throw new Error(MessageFormat.format(Messages.BehaviorGuardEvaluator_1, event), e);
		} catch (InvocationTargetException e) {
			if (e.getCause() instanceof Error) {
				throw (Error) e.getCause();
			}
			throw e;
		}
	}

	/**
	 * Returns he object instance containing the {@code PerceptGuardEvaluator}.
	 * @return the object instance containing the {@code PerceptGuardEvaluator}
	 */
	public Object getTarget() {
		return this.target;
	}

	@Override
	public int hashCode() {
		return (31 + this.method.hashCode()) * 31 + System.identityHashCode(this.target);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BehaviorGuardEvaluator) {
			final BehaviorGuardEvaluator that = (BehaviorGuardEvaluator) obj;
			// Use == so that different equal instances will still receive events.
			// We only guard against the case that the same object is registered
			// multiple times
			return this.target == that.target && this.method.equals(that.method);
		}
		return false;
	}

}
