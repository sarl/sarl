/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.lang.core.tests.core;

import static io.sarl.tests.api.tools.TestAssertions.assertException;
import static io.sarl.tests.api.tools.TestAssertions.assertInstanceOf;
import static io.sarl.tests.api.tools.TestMockito.spy;
import static io.sarl.tests.api.tools.TestReflections.invokeFunc;
import static io.sarl.tests.api.tools.TestReflections.invokeProc;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.security.InvalidParameterException;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.AbstractSkillContainer;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.AtomicSkillReference;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.DefaultSkill;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SREutils;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("AbstractSkillContainer")
@Tag("unit")
@Tag("core")
public class AbstractSkillContainerTest extends AbstractSarlTest {

	private UUID id;

	private AbstractSkillContainerMock container;
	
	private void assertNoSkill(Class<? extends Capacity> c) throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			invokeProc(this.container.getClass(), this.container, "getSkill", new Class[] {Class.class}, c);
			invokeProc(this.container.getClass(), this.container, "getSkill", new Class[] {Class.class}, c);
		});
	}

	private void assertSkill(Class<? extends Capacity> c, Skill expected) throws Exception {
		Object r = invokeFunc(this.container.getClass(), this.container, Skill.class,
				"getSkill", new Class[] {Class.class}, c);
		assertSame(expected, r);
	}

	private void assertSkill(Class<? extends Capacity> c) throws Exception {
		Object r = invokeFunc(this.container.getClass(), this.container, Skill.class,
				"getSkill", new Class[] {Class.class}, c);
		assertNotNull(r);
	}

	@BeforeEach
	public void setUp() {
		this.id = UUID.randomUUID();
		this.container = spy(new AbstractSkillContainerMock(this.id));
	}

	@Test
	public void setSkill() throws Exception {
		Skill s1, s2, r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		r = this.container.setSkill_Fake(s1, Capacity1.class);
		assertSame(s1, r);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkill_Fake(new Skill2(), Capacity1.class);
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkill_Fake(new Skill3(), Capacity1.class);
		});

		s2 = new Skill2();
		r = this.container.setSkill_Fake(s2, Capacity2.class);
		assertSame(s2, r);
		assertSkill(Capacity1.class, s1);
		assertSkill(Capacity2.class, s2);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkill_Fake(new Skill1(), Capacity2.class);
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkill_Fake(new Skill3(), Capacity2.class);
		});
	}

	@Test
	public void setSkill_withoutCapacity() throws Exception {
		Skill s4, r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
		assertNoSkill(Skill4.class);

		s4 = new Skill4();
		r = this.container.setSkill_Fake(s4);
		
		assertSame(s4, r);
		assertSkill(Capacity1.class, s4);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void setSkillIfAbsent() throws Exception {
		Skill s1, s2;
		Object r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		this.container.setSkillIfAbsent_Fake(s1, Capacity1.class);
		r = this.container.getSkill_Fake(Capacity1.class);
		assertSame(s1, r);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkillIfAbsent_Fake(new Skill2(), Capacity1.class);
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkillIfAbsent_Fake(new Skill3(), Capacity1.class);
		});

		s2 = new Skill4();
		this.container.setSkillIfAbsent_Fake(s2, Capacity1.class);
		r = this.container.getSkill_Fake(Capacity1.class);
		assertSame(s1, r);

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkillIfAbsent_Fake(new Skill1(), Capacity2.class);
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.setSkillIfAbsent_Fake(new Skill3(), Capacity2.class);
		});
	}

	@Test
	public void setSkillIfAbsent_withoutCapacity() throws Exception {
		Skill s4;
		Object r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
		assertNoSkill(Skill4.class);

		s4 = new Skill4();
		this.container.setSkillIfAbsent_Fake(s4);
		r = this.container.getSkill_Fake(Capacity2.class);
		
		assertSame(s4, r);
		assertSkill(Capacity1.class, s4);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		Skill s2 = new Skill2();
		this.container.setSkillIfAbsent_Fake(s2);
		r = this.container.getSkill_Fake(Capacity2.class);
		assertSame(s4, r);
}

	@Test
	public void clearSkill_multipleCapacityImplementation() throws Exception {
		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
		assertNoSkill(Skill4.class);
		Skill s4 = new Skill4();
		this.container.setSkill_Fake(s4);
		assertSkill(Capacity1.class, s4);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.container.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		// again
		this.container.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.container.clearSkill(Capacity2.class);

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void clearSkill() throws Exception {
		this.container.setSkill_Fake(new Skill1(), Capacity1.class);
		this.container.setSkill_Fake(new Skill2(), Capacity2.class);
		assertSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.container.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		// again
		this.container.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.container.clearSkill(Capacity2.class);

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void hasSkill() throws Exception {
		this.container.setSkill_Fake(new Skill1(), Capacity1.class);
		this.container.setSkill_Fake(new Skill2(), Capacity2.class);
		assertTrue(this.container.hasSkill(Capacity1.class));
		assertTrue(this.container.hasSkill(Capacity2.class));

		this.container.clearSkill(Capacity1.class);

		assertFalse(this.container.hasSkill(Capacity1.class));
		assertTrue(this.container.hasSkill(Capacity2.class));
		assertFalse(this.container.hasSkill(Skill1.class));
		assertFalse(this.container.hasSkill(Skill2.class));

		// again
		this.container.clearSkill(Capacity1.class);

		assertFalse(this.container.hasSkill(Capacity1.class));
		assertTrue(this.container.hasSkill(Capacity2.class));
		assertFalse(this.container.hasSkill(Skill1.class));
		assertFalse(this.container.hasSkill(Skill2.class));

		this.container.clearSkill(Capacity2.class);

		assertFalse(this.container.hasSkill(Capacity1.class));
		assertFalse(this.container.hasSkill(Capacity2.class));
		assertFalse(this.container.hasSkill(Skill1.class));
		assertFalse(this.container.hasSkill(Skill2.class));
	}

	@Test
	public void operator_mappedTo() throws Exception {
		Skill s1, s2;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		this.container.operator_mappedTo(Capacity1.class, s1);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		assertException(InvalidParameterException.class, () -> {
			this.container.operator_mappedTo(Capacity1.class, new Skill2());
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.operator_mappedTo(Capacity1.class, new Skill3());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		});

		s2 = new Skill2();
		this.container.operator_mappedTo(Capacity2.class, s2);
		assertSkill(Capacity1.class, s1);
		assertSkill(Capacity2.class, s2);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		assertException(InvalidParameterException.class, () -> {
			this.container.operator_mappedTo(Capacity2.class, new Skill1());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		});

		assertException(InvalidParameterException.class, () -> {
			this.container.operator_mappedTo(Capacity2.class, new Skill3());
		});
	}

	@Test
	public void skillInstallation_withMultipleSetSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.container.setSkill_Fake(s4, Capacity1.class);
		
		assertEquals(1, s4.installCalls());
		
		this.container.setSkill_Fake(s4, Capacity1.class);

		assertEquals(0, s4.installCalls());
	}

	@Test
	public void skillInstallation_withSingleSetSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.container.setSkill_Fake(s4);
		assertEquals(1, s4.installCalls());
	}

	@Test
	public void skillUninstallation_withClearSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.container.setSkill_Fake(s4);

		this.container.clearSkill(Capacity1.class);
		assertEquals(0, s4.uninstallPreCalls());
		assertEquals(0, s4.uninstallPostCalls());

		this.container.clearSkill(Capacity2.class);
		assertEquals(1, s4.uninstallPreCalls());
		assertEquals(1, s4.uninstallPostCalls());
	}

	@Test
	public void getSkill_noSetSkill() throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity1.class);
		});
	}

	@Test
	public void getSkill_setSkill() throws Exception {
		Skill1 so = new Skill1();
		this.container.setSkill_Fake(so);
		//
		AtomicSkillReference ref0 = this.container.$getSkill(Capacity1.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertSame(so, s0);
		//
		AtomicSkillReference ref1 = this.container.$getSkill(Capacity1.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	@Test
	public void getSkill_defaultskill() throws Exception {
		AtomicSkillReference ref0 = this.container.$getSkill(Capacity3.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertNotNull(s0);
		assertInstanceOf(Skill5.class, s0);
		//
		AtomicSkillReference ref1 = this.container.$getSkill(Capacity3.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	@Test
	public void getSkill_noRegistration() throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill6() throws Exception {
		Skill expected = new Skill6();
		SREutils.setInternalSkill(this.container, expected, new Class[0]);
		//
		AtomicSkillReference actual = this.container.$getSkill(Capacity4.class);
		assertNotNull(actual);
		assertSame(expected, actual.get());
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7() throws Exception {
		Skill expected = new Skill7();
		SREutils.setInternalSkill(this.container, expected, new Class[0]);
		//
		AtomicSkillReference actual0 = this.container.$getSkill(Capacity4.class);
		assertNotNull(actual0);
		assertSame(expected, actual0.get());
		//
		AtomicSkillReference actual1 = this.container.$getSkill(Capacity5.class);
		assertNotNull(actual1);
		assertSame(expected, actual1.get());
	}

	@Test
	public void getSkill_registrationSkill6_clearReference() throws Exception {
		Skill expected = new Skill6();
		SREutils.setInternalSkill(this.container, expected, new Class[0]).clear();
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7_clearReference0() throws Exception {
		Skill expected = new Skill7();
		// The following line clear the Capacity5 reference only.
		SREutils.setInternalSkill(this.container, expected, new Class[0]).clear();
		//
		AtomicSkillReference actual = this.container.$getSkill(Capacity4.class);
		assertNotNull(actual);
		assertSame(expected, actual.get());
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7_clearReference1() throws Exception {
		Skill expected = new Skill7();
		// The following line clear the Capacity5 reference only.
		SREutils.setInternalSkill(this.container, expected, new Class[0]).clear();
		// The following line clear the Capacity4 reference only.
		SREutils.getInternalSkillReference(this.container, Capacity4.class).clear();
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.container.$getSkill(Capacity5.class);
		});
	}

	/** Only for making public several protected methods.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AbstractSkillContainerMock extends AbstractSkillContainer {

		private final UUID id;
		
		public AbstractSkillContainerMock(UUID id) {
			super(null);
			this.id = id;
		}

		public <S extends Capacity> S getSkill_Fake(Class<S> capacity) {
			return getSkill(capacity);
		}

		public <S extends Skill> S setSkill_Fake(S skill, Class<? extends Capacity>... capacity) {
			return setSkill(skill, capacity);
		}
		
		public void setSkillIfAbsent_Fake(Skill skill, Class<? extends Capacity>... capacity) {
			setSkillIfAbsent(skill, capacity);
		}

		@Override
		public AtomicSkillReference $getSkill(Class<? extends Capacity> capacity) {
			return super.$getSkill(capacity);
		}

		@Override
		public <S extends Capacity> S clearSkill(Class<S> capacity) {
			return super.clearSkill(capacity);
		}

		@Override
		public boolean hasSkill(Class<? extends Capacity> capacity) {
			return super.hasSkill(capacity);
		}

		@Override
		public <S extends Skill> void operator_mappedTo(
				Class<? extends Capacity> capacity, S skill) {
			super.operator_mappedTo(capacity, skill);
		}

		@Override
		public UUID getID() {
			return this.id;
		}

		@Override
		protected void $attachOwner(Skill skill) {
			//
		}

		@Override
		protected void toString(ToStringBuilder builder) {
			throw new UnsupportedOperationException();
		}

		@Override
		protected boolean isMe(Address address) {
			throw new UnsupportedOperationException();
		}

		@Override
		protected boolean isMe(UUID uID) {
			throw new UnsupportedOperationException();
		}

		@Override
		protected boolean isFromMe(Event event) {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity1 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity2 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DefaultSkill(Skill5.class)
	private static interface Capacity3 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill1 extends Skill implements Capacity1 {
		public Skill1() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill2 extends Skill implements Capacity2 {
		public Skill2() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill3 extends Skill {
		public Skill3() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity4 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity5 extends Capacity4 {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill6 extends Skill implements Capacity4 {
		public Skill6() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill7 extends Skill implements Capacity5 {
		public Skill7() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill4 extends Skill1 implements Capacity2 {
		private final AtomicInteger installCalls = new AtomicInteger();
		private final AtomicInteger uninstallPreCalls = new AtomicInteger();
		private final AtomicInteger uninstallPostCalls = new AtomicInteger();
		public Skill4() {
			//
		}
		@Override
		protected void install() {
			super.install();
			this.installCalls.incrementAndGet();
		}
		@Override
		protected void prepareUninstallation() {
			this.uninstallPreCalls.incrementAndGet();
			super.prepareUninstallation();
		}
		@Override
		protected void uninstall() {
			this.uninstallPostCalls.incrementAndGet();
			super.uninstall();
		}
		public int installCalls() {
			return this.installCalls.getAndSet(0);
		}
		public int uninstallPreCalls() {
			return this.uninstallPreCalls.getAndSet(0);
		}
		public int uninstallPostCalls() {
			return this.uninstallPostCalls.getAndSet(0);
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill5 extends Skill implements Capacity3 {
		public Skill5() {
			//
		}
	}

}
