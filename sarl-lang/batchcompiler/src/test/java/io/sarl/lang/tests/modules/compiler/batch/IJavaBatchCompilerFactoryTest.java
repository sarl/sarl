/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.lang.tests.modules.compiler.batch;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ServiceLoader;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.compiler.batch.IJavaBatchCompilerFactory;
import io.sarl.lang.compiler.batch.JavacBatchCompilerFactory;


/**
 * @author $Author: sgalland$
 * @version batchcompiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid batchcompiler
 */
@SuppressWarnings("all")
@DisplayName("IJavaBatchCompilerFactory service")
@Tag("core")
@Tag("compiler-run")
public class IJavaBatchCompilerFactoryTest {

	@Test
	public void getService() throws Exception {
		var loader = ServiceLoader.load(IJavaBatchCompilerFactory.class);
		assertNotNull(loader);
		var iterator = loader.iterator();
		assertNotNull(iterator);
		assertTrue(iterator.hasNext());
		var factory = iterator.next();
		assertNotNull(factory);
		assertInstanceOf(JavacBatchCompilerFactory.class, factory);
	}

}
