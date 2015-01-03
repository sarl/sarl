/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.tests.util;

import static org.junit.Assert.*;
import io.sarl.eclipse.util.AttributeUtil;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class AttributeUtilTest extends AbstractSarlTest {

	@Nullable
	private Map<String, String> mapStr;
	
	@Before
	public void setUp() {
		this.mapStr = new HashMap<>();
		Random random = new Random();
		int nb = random.nextInt(25);
		for (int i = 0; i < nb; ++i) {
			String value;
			switch (random.nextInt(3)) {
			case 0:
				value = Boolean.TRUE.toString();
				break;
			case 1:
				value = Boolean.FALSE.toString();
				break;
			default:
				value = UUID.randomUUID().toString();
				break;
			}
			this.mapStr.put(UUID.randomUUID().toString(), value);
		}
		for (int i = 0; i < nb; ++i) {
			this.mapStr.put(UUID.randomUUID().toString(), null);
		}
	}
	
	@Test
	public void getMapStringString() {
		String defaultValue = UUID.randomUUID().toString();
		for (Entry<String, String> entry : this.mapStr.entrySet()) {
			String value = AttributeUtil.get(this.mapStr, entry.getKey(), defaultValue);
			if (entry.getValue() == null) {
				assertSame(defaultValue, value);
			} else {
				assertSame(entry.getValue(), value);
			}
		}
	}

	@Test
	public void getMapStringBoolean() {
		boolean defaultValue = true;
		for (Entry<String, String> entry : this.mapStr.entrySet()) {
			boolean value = AttributeUtil.get(this.mapStr, entry.getKey(), defaultValue);
			if (Boolean.TRUE.toString().equals(entry.getValue())) {
				assertTrue(value);
			} else if (Boolean.FALSE.toString().equals(entry.getValue())) {
				assertFalse(value);
			} else {
				assertEquals("Map value is: " + entry.getValue(), defaultValue, value);
			}
		}
	}

}
