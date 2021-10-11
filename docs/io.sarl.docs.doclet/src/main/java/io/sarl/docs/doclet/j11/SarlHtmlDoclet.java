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

package io.sarl.docs.doclet.j11;

import java.util.ArrayList;
import java.util.List;

import javax.lang.model.element.PackageElement;

import com.google.inject.Injector;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.internal.doclets.formats.html.HtmlDoclet;
import jdk.javadoc.internal.doclets.toolkit.AbstractDoclet;
import jdk.javadoc.internal.doclets.toolkit.DocletException;
import jdk.javadoc.internal.doclets.toolkit.util.ClassTree;
import jdk.javadoc.internal.doclets.toolkit.util.DocPaths;

import io.sarl.docs.doclet.j11.config.SarlHtmlConfiguration;
import io.sarl.docs.doclet.j11.factories.DeprecatedListWriter;
import io.sarl.docs.doclet.j11.factories.DeprecatedListWriterFactory;
import io.sarl.docs.doclet.j11.factories.PackageTreeWriter;
import io.sarl.docs.doclet.j11.factories.PackageTreeWriterFactory;
import io.sarl.docs.doclet.j11.factories.TreeWriter;
import io.sarl.docs.doclet.j11.factories.TreeWriterFactory;
import io.sarl.docs.doclet.j11.utils.Reflect;
import io.sarl.docs.doclet.j11.writers.SarlDeprecatedListWriter;
import io.sarl.docs.doclet.j11.writers.SarlPackageTreeWriter;
import io.sarl.docs.doclet.j11.writers.SarlTreeWriter;

/** SARL Doclet that is generated the HTML documentation.
 *
 * <p>This version of the SARL doc let is an adaptation of the
 * previous SARL doclet (for Java 8) to Java 11 and higher API.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Inversion of Control</li>
 * <li>Force the configuration to be of type {@link SarlHtmlConfiguration}</li>
 * <li>Force the writer of deprecation list to be of type {@link SarlDeprecatedListWriter}</li>
 * <li>Force the writer of class hierarchy to be of type {@link SarlTreeWriter}</li>
 * <li>Force the writer of package's class hierarchy to be of type {@link SarlPackageTreeWriter}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlHtmlDoclet extends HtmlDoclet {

	protected final Injector injector;

	private SarlHtmlConfiguration configuration;

	/** Constructor.
	 *
	 * @param parent the parent doclet.
	 * @param injector the injector.
	 */
	public SarlHtmlDoclet(Doclet parent, Injector injector) {
		super(parent);
		this.injector = injector;
		setConfiguration(createSarlHtmlConfiguration());
		// The following line is a hack for avoiding the internal exception that
		// avoids the HtmlDoclet to be overridden
		Reflect.setField(AbstractDoclet.class, "TOOLKIT_DOCLET_NAME", getClass().getName());
	}

	/** Create the instance of the configuration to be used.
	 * This is a factory method.
	 *
	 * @return the configuration instance.
	 */
	protected SarlHtmlConfiguration createSarlHtmlConfiguration() {
		final SarlHtmlConfiguration instance = new SarlHtmlConfiguration(this, this.injector);
		return instance;
	}

	@Override
	public SarlHtmlConfiguration getConfiguration() {
		return this.configuration;
	}

	/** Change the configuration instance.
	 *
	 * @param configuration the instance.
	 */
	protected void setConfiguration(SarlHtmlConfiguration configuration) {
		Reflect.setField(this, AbstractDoclet.class, "configuration", configuration);
		Reflect.setField(this, HtmlDoclet.class, "configuration", configuration);
		this.configuration = configuration;
	}

	@Override
	public String getName() {
		return "SARL-HTML";
	}

	@Override
	protected void generateOtherFiles(DocletEnvironment docEnv, ClassTree classtree) throws DocletException {
		super.generateOtherFiles(docEnv, classtree);

		// Re-generate the deprecation list
		if (!this.configuration.nodeprecatedlist && !this.configuration.nodeprecated) {
			final DeprecatedListWriter writer = this.injector.getInstance(DeprecatedListWriterFactory.class).create(this.configuration, DocPaths.DEPRECATED_LIST);
			writer.generatePage();        
		}

		// Re-generate the overview of the class hierarchy
		if (this.configuration.createtree) {
			final TreeWriter writer = this.injector.getInstance(TreeWriterFactory.class).create(this.configuration, classtree);
			writer.generatePage();        
        }
	}

	@Override
    protected void generatePackageFiles(ClassTree classtree) throws DocletException {
		super.generatePackageFiles(classtree);

		// Re-generate the overview of the package's class hierarchy
        final List<PackageElement> pList = new ArrayList<>(this.configuration.packages);
        for (PackageElement pkg : pList) {
            if ((!this.configuration.nodeprecated || !this.utils.isDeprecated(pkg))
            		&& this.configuration.createtree) {
    			final PackageTreeWriter writer = this.injector.getInstance(PackageTreeWriterFactory.class).create(this.configuration, pkg, this.configuration.nodeprecated);
    			writer.generatePage();        
            }
        }
    }

}
