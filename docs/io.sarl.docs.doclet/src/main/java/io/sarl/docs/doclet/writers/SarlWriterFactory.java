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

package io.sarl.docs.doclet.writers;

import java.io.IOException;

import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.Type;
import com.sun.tools.doclets.formats.html.ConstructorWriterImpl;
import com.sun.tools.doclets.formats.html.FieldWriterImpl;
import com.sun.tools.doclets.formats.html.MethodWriterImpl;
import com.sun.tools.doclets.formats.html.SubWriterHolderWriter;
import com.sun.tools.doclets.formats.html.WriterFactoryImpl;
import com.sun.tools.doclets.internal.toolkit.AnnotationTypeWriter;
import com.sun.tools.doclets.internal.toolkit.ClassWriter;
import com.sun.tools.doclets.internal.toolkit.util.ClassTree;

import io.sarl.docs.doclet.SarlConfiguration;

/** Class builder dedicated to the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlWriterFactory extends WriterFactoryImpl {

	/** Configuration.
	 */
	protected final SarlConfiguration configuration;

	/** Constructor.
	 *
	 * @param configuration the associated configuration.
	 */
	public SarlWriterFactory(SarlConfiguration configuration) {
		super(configuration);
		this.configuration = configuration;
	}

	@Override
	public ConstructorWriterImpl getConstructorWriter(ClassWriter classWriter) throws Exception {
		return new SarlConstructorWriter((SubWriterHolderWriter) classWriter,
				classWriter.getClassDoc());
	}

	@Override
	public MethodWriterImpl getMethodWriter(ClassWriter classWriter) throws Exception {
		return new SarlMethodWriter((SubWriterHolderWriter) classWriter,
				classWriter.getClassDoc());
	}

	@Override
	public FieldWriterImpl getFieldWriter(ClassWriter classWriter) throws Exception {
		return new SarlFieldWriter((SubWriterHolderWriter) classWriter,
				classWriter.getClassDoc());
	}

	@Override
	public ClassWriter getClassWriter(ClassDoc classDoc, ClassDoc prevClass, ClassDoc nextClass, ClassTree classTree)
			throws IOException {
		return new SarlClassWriter(this.configuration, classDoc, prevClass, nextClass, classTree);
	}

	@Override
	public AnnotationTypeWriter getAnnotationTypeWriter(AnnotationTypeDoc annotationType, Type prevType, Type nextType)
			throws Exception {
		return new SarlAnnotationTypeWriter(this.configuration, annotationType, prevType, nextType);
	}

}
