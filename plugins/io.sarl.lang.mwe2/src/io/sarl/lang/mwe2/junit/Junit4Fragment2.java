/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.mwe2.junit;

import com.google.inject.Inject;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.xtext.generator.CodeConfig;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * A fixed version of the Junit4 fragment that is able to skip the generation of injectors or the example.
 *
 * FIXME: Remove when Xtext has fixed the issue <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=484315">484315</a>.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class Junit4Fragment2 extends org.eclipse.xtext.xtext.generator.junit.Junit4Fragment2 {

	private boolean generateStub = true;

	@Inject
	private CodeConfig codeConfig;

	/** Change the flag for generating the Junit4 example.
	 *
	 * @param generate <code>true</code> for generating the example.
	 */
	public void setGenerateStub(boolean generate) {
		this.generateStub = generate;
	}

	/** Replies the flag for generating the Junit4 example.
	 *
	 * @return <code>true</code> for generating the example.
	 */
	public boolean isGenerateStub() {
		return this.generateStub;
	}

	@Override
	public JavaFileAccess generateExampleRuntimeTest() {
		if (isGenerateStub()) {
			return super.generateExampleRuntimeTest();
		}
		return new NoFile(exampleRuntimeTest(), this.codeConfig);
	}
	
	/**
	 * A file object that is ignore anny request for being written on the disk.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class NoFile extends JavaFileAccess {

		/**
		 * @param typeRef
		 * @param codeConfig
		 */
		protected NoFile(TypeReference typeRef, CodeConfig codeConfig) {
			super(typeRef, codeConfig);
		}
		
		@Override
		public void writeTo(IFileSystemAccess2 fileSystemAccess) {
			//
		}

	}

}
