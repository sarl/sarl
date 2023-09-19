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
package io.sarl.lang.tests.modules.compiler.batch;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;

import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.xtext.validation.Issue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.compiler.batch.IJavaBatchCompilerFactory;
import io.sarl.lang.compiler.batch.JavacBatchCompilerFactory;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;


/**
 * @author $Author: sgalland$
 * @version batchcompiler 0.13.0 20230919-093056
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
		ServiceLoader<IJavaBatchCompilerFactory> loader = ServiceLoader.load(IJavaBatchCompilerFactory.class);
		assertNotNull(loader);
		Iterator<IJavaBatchCompilerFactory> iterator = loader.iterator();
		assertNotNull(iterator);
		assertTrue(iterator.hasNext());
		IJavaBatchCompilerFactory factory = iterator.next();
		assertNotNull(factory);
		assertInstanceOf(JavacBatchCompilerFactory.class, factory);
	}

}
