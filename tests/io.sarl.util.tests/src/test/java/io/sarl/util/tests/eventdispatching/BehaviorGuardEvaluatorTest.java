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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests.eventdispatching;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.inject.Inject;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.sarl.eventdispatching.BehaviorGuardEvaluator;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorGuardEvaluatorTest extends AbstractSarlTest {

	@Inject
	private ReflectExtensions reflect;
	
	@Mock
	private ListenerObject listener;

	@Nullable
	private Method handler;

	@Nullable
	private BehaviorGuardEvaluator evaluator;

	@Before
	public void setUp() throws Exception {
		this.handler = ListenerObject.class.getDeclaredMethod("theFunction", Event.class, Collection.class);
		this.evaluator = new BehaviorGuardEvaluator(this.listener, this.handler);
	}

	@Test
	public void getTarget() {
		assertSame(this.listener, this.evaluator.getTarget());
	}

	@Test
	public void testEquals_null() {
		assertFalse(this.evaluator.equals(null));
	}

	@Test
	public void testEquals_object() {
		assertFalse(this.evaluator.equals(new Object()));
	}

	@Test
	public void testEquals_this() {
		assertTrue(this.evaluator.equals(this.evaluator));
	}

	@Test
	public void testEquals_sameEvaluator() {
		assertTrue(this.evaluator.equals(new BehaviorGuardEvaluator(this.listener, this.handler)));
	}

	@Test
	public void testEquals_otherEvaluator0() throws Exception {
		assertFalse(this.evaluator.equals(new BehaviorGuardEvaluator(this.listener, Object.class.getDeclaredMethod("toString"))));
	}

	@Test
	public void testEquals_otherEvaluator1() throws Exception {
		assertFalse(this.evaluator.equals(new BehaviorGuardEvaluator(new Object(), this.handler)));
	}

	@Test
	public void testEquals_otherEvaluator2() throws Exception {
		assertFalse(this.evaluator.equals(new BehaviorGuardEvaluator(new Object(), Object.class.getDeclaredMethod("toString"))));
	}

	@Test
	public void evaluateGuard() throws Exception {
		List<Runnable> runs = new ArrayList<>();
		this.evaluator.evaluateGuard(Mockito.mock(Event.class), runs);
		assertEquals(1, runs.size());
		assertSame(this.listener.getRunnable(), runs.get(0));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ListenerObject {

		private final Runnable run = Mockito.mock(Runnable.class);

		public Runnable getRunnable() {
			return this.run;
		}

		private void theFunction(Event event, Collection<Runnable> callers) {
			callers.add(this.run);
		}

	}
	
}
