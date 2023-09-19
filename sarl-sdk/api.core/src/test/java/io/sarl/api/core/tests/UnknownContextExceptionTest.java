/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.api.core.tests;

import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.api.core.UnknownContextException;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.13.0 20230919-093058
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@DisplayName("UnknownContextException")
@Tag("unit")
@Tag("api")
public class UnknownContextExceptionTest extends AbstractSarlTest {

	private UUID id;

	private UnknownContextException exception;

	/**
	 */
	@BeforeEach
	public void setUp() {
		this.id = UUID.randomUUID();
		this.exception = new UnknownContextException(this.id);
	}

	/**
	 */
	@Test
	public void getUnknownContextID() {
		assertSame(this.id, this.exception.getUnknownContextID());
	}

}
