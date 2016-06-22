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
package io.sarl.lang.core.tests.scoping.batch;

import static io.sarl.lang.scoping.batch.SARLTimeExtensions.*;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.google.common.annotations.GwtCompatible;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.scoping.batch.SARLTimeExtensions.TimeExtensionsConstants;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.eclipse.xtext.xbase.lib.Pair;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLTimeExtensionsTest {

	private static void assertSimilar(Number expected, Number actual) {
		assertEquals(expected.longValue(), actual.longValue());
	}
	
	@Test
	public void testMilliseconds() {
		assertSimilar(1234, milliseconds(1234));
		assertSimilar(-234, milliseconds(-234));
	}

	@Test
	public void testSeconds() {
		assertSimilar(1234000, seconds(1234));
		assertSimilar(-234000, seconds(-234));
	}

	@Test
	public void testMinutes() {
		assertSimilar(74040000, minutes(1234));
		assertSimilar(-14040000, minutes(-234));
	}

	@Test
	public void testHours() {
		assertSimilar(4442400000l, hours(1234));
		assertSimilar(-842400000l, hours(-234));
	}

	@Test
	public void testDays() {
		assertSimilar(106617600000l, days(1234));
		assertSimilar(-20217600000l, days(-234));
	}

	@Test
	public void testWeeks() {
		assertSimilar(746323200000l, weeks(1234));
		assertSimilar(-141523200000l, weeks(-234));
	}

}
