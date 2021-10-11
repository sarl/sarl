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

package io.sarl.docs.doclet.j11.writers;

import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import jdk.javadoc.internal.doclets.formats.html.ConstructorWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.FieldWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.MethodWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.WriterFactoryImpl;
import jdk.javadoc.internal.doclets.toolkit.AnnotationTypeFieldWriter;
import jdk.javadoc.internal.doclets.toolkit.AnnotationTypeOptionalMemberWriter;
import jdk.javadoc.internal.doclets.toolkit.AnnotationTypeRequiredMemberWriter;
import jdk.javadoc.internal.doclets.toolkit.AnnotationTypeWriter;
import jdk.javadoc.internal.doclets.toolkit.ClassWriter;
import jdk.javadoc.internal.doclets.toolkit.PackageSummaryWriter;
import jdk.javadoc.internal.doclets.toolkit.util.ClassTree;

import io.sarl.docs.doclet.j11.factories.AnnotationTypeFieldWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeOptionalMemberWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeRequiredMemberWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeWriterFactory;
import io.sarl.docs.doclet.j11.factories.ClassWriterFactory;
import io.sarl.docs.doclet.j11.factories.ConstructorWriterFactory;
import io.sarl.docs.doclet.j11.factories.FieldWriterFactory;
import io.sarl.docs.doclet.j11.factories.MethodWriterFactory;
import io.sarl.docs.doclet.j11.factories.PackageWriterFactory;

/** Class builder dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Call the factories for each writer that has one into the SARL doclet</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlWriterFactory extends WriterFactoryImpl {

    private final HtmlConfiguration configuration;

    private final ClassWriterFactory classWriterFactory;
	
    private final AnnotationTypeWriterFactory annotationTypeWriterFactory;

    private final FieldWriterFactory fieldWriterFactory;
	
    private final AnnotationTypeFieldWriterFactory annotationTypeFieldWriterFactory;

    private final AnnotationTypeOptionalMemberWriterFactory annotationTypeOptionalMemberWriterFactory;

    private final AnnotationTypeRequiredMemberWriterFactory annotationTypeRequiredMemberWriterFactory;

    private final MethodWriterFactory methodWriterFactory;

	private final ConstructorWriterFactory constructorWriterFactory;

	private final PackageWriterFactory packageWriterFactory;

	/** Constructor.
	 *
	 * @param configuration the associated configuration.
	 * @param classWriterFactory the factory of class writer.
	 * @param annotationTypeWriterFactory the factory of annotation type writer.
	 * @param fieldWriterFactory the factory of field writer.
	 * @param annotationTypeFieldWriterFactory the factory of annotation type's field writer.
	 * @param annotationTypeOptionalMemberWriterFactory the factory of annotation type's optional member writer.
	 * @param annotationTypeRequiredMemberWriterFactory the factory of annotation type's required member writer.
	 * @param methodWriterFactory the factory of method writer.
	 * @param constructorWriterFactory the factory of constructor writer.
	 * @param packageWriterFactory the factory of the package summary.
	 */
	public SarlWriterFactory(HtmlConfiguration configuration,
			ClassWriterFactory classWriterFactory,
			AnnotationTypeWriterFactory annotationTypeWriterFactory,
			FieldWriterFactory fieldWriterFactory,
			AnnotationTypeFieldWriterFactory annotationTypeFieldWriterFactory,
			AnnotationTypeOptionalMemberWriterFactory annotationTypeOptionalMemberWriterFactory,
			AnnotationTypeRequiredMemberWriterFactory annotationTypeRequiredMemberWriterFactory,
			MethodWriterFactory methodWriterFactory,
			ConstructorWriterFactory constructorWriterFactory,
			PackageWriterFactory packageWriterFactory) {
		super(configuration);
		this.configuration = configuration;
		this.classWriterFactory = classWriterFactory;
		this.annotationTypeWriterFactory = annotationTypeWriterFactory;
		this.fieldWriterFactory = fieldWriterFactory;
		this.annotationTypeFieldWriterFactory = annotationTypeFieldWriterFactory;
		this.annotationTypeOptionalMemberWriterFactory = annotationTypeOptionalMemberWriterFactory;
		this.annotationTypeRequiredMemberWriterFactory = annotationTypeRequiredMemberWriterFactory;
		this.methodWriterFactory = methodWriterFactory;
		this.constructorWriterFactory = constructorWriterFactory;
		this.packageWriterFactory = packageWriterFactory;
	}

	@Override
	public ClassWriter getClassWriter(TypeElement typeElement, ClassTree classTree) {
		return this.classWriterFactory.create(this.configuration, typeElement, classTree);
	}

	@Override
	public AnnotationTypeWriter getAnnotationTypeWriter(TypeElement annotationType) {
		return this.annotationTypeWriterFactory.create(this.configuration, annotationType);
	}

	@Override
	public FieldWriterImpl getFieldWriter(ClassWriter classWriter) {
		return (FieldWriterImpl) this.fieldWriterFactory.create(classWriter);
	}

	@Override
	public AnnotationTypeFieldWriter getAnnotationTypeFieldWriter(AnnotationTypeWriter annotationTypeWriter) {
		final TypeElement te = annotationTypeWriter.getAnnotationTypeElement();
		return this.annotationTypeFieldWriterFactory.create((SubWriterHolderWriter) annotationTypeWriter, te);
	}

	@Override
	public AnnotationTypeOptionalMemberWriter getAnnotationTypeOptionalMemberWriter(
			AnnotationTypeWriter annotationTypeWriter) {
        final TypeElement te = annotationTypeWriter.getAnnotationTypeElement();
		return this.annotationTypeOptionalMemberWriterFactory.create((SubWriterHolderWriter) annotationTypeWriter, te);
	}

	@Override
	public AnnotationTypeRequiredMemberWriter getAnnotationTypeRequiredMemberWriter(
			AnnotationTypeWriter annotationTypeWriter) {
        final TypeElement te = annotationTypeWriter.getAnnotationTypeElement();
		return this.annotationTypeRequiredMemberWriterFactory.create((SubWriterHolderWriter) annotationTypeWriter, te);
	}

	@Override
	public MethodWriterImpl getMethodWriter(ClassWriter classWriter) {
		return (MethodWriterImpl) this.methodWriterFactory.create(classWriter);
	}

	@Override
	public ConstructorWriterImpl getConstructorWriter(ClassWriter classWriter) {
		return (ConstructorWriterImpl) this.constructorWriterFactory.create(classWriter);
	}

	@Override
	public PackageSummaryWriter getPackageSummaryWriter(PackageElement packageElement) {
		return this.packageWriterFactory.create(this.configuration, packageElement);
	}

}
