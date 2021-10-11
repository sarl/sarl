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

import java.util.Arrays;
import java.util.Locale;
import java.util.Set;

import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;

import com.google.inject.Injector;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.internal.tool.DocEnvImpl;

import io.sarl.docs.doclet.j11.config.ApidocExcluder;
import io.sarl.docs.doclet.j11.guicemodules.SarlJavadocModule;
import io.sarl.lang.SARLStandaloneSetup;

/** SARL Doclet that is generated the HTML documentation.
 *
 * <p>This version of the SARL doc let is an adaptation of the
 * previous SARL doclet (for Java 8) to Java 11 and higher API.
 *
 * <p>This version of the Doclet uses Inversion of Control.
 *
 * <p>This doclet also ignore the hidden features according to the SARL specification.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlDoclet implements Doclet {

	private Injector mainInjector;

	private Injector injector;
	
	private SarlHtmlDoclet doclet;

	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	protected Injector getInjector() {
		if (this.injector == null) {
			this.mainInjector = SARLStandaloneSetup.doSetup();
			this.injector = this.mainInjector.createChildInjector(Arrays.asList(new SarlJavadocModule(this)));
		}
		return this.injector;
	}
	
	/** Replies the injected doclet.
	 *
	 * @return the injected doclet.
	 */
	protected SarlHtmlDoclet getInjectedDoclet() {
		if (this.doclet == null) {
			this.doclet = getInjector().getInstance(SarlHtmlDoclet.class);
		}
		return this.doclet;
	}

	@Override
	public void init(Locale locale, Reporter reporter) {
		getInjectedDoclet().init(locale, reporter);
	}

	@Override
	public String getName() {
		return getInjectedDoclet().getName();
	}

	@Override
	public Set<? extends Option> getSupportedOptions() {
		return getInjectedDoclet().getSupportedOptions();
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return getInjectedDoclet().getSupportedSourceVersion();
	}

	@Override
	public boolean run(DocletEnvironment environment) {
		if (environment instanceof DocEnvImpl) {
			final ApidocExcluder excluder = getInjector().getInstance(ApidocExcluder.class);
			return getInjectedDoclet().run(new SarlDocletEnvironment((DocEnvImpl) environment, excluder));
		}
		return getInjectedDoclet().run(environment);
	}

	/** SARL doclet environment.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	private static class SarlDocletEnvironment extends DocEnvImpl {

		private final ApidocExcluder excluder;
		
		/** Constructor.
		 *
		 * @param parent the parent.
		 * @param excluder the excluder.
		 */
		SarlDocletEnvironment(DocEnvImpl parent, ApidocExcluder excluder) {
			super(parent.toolEnv, parent.etable);
			this.excluder = excluder;
		}

		@Override
		public boolean isIncluded(Element element) {
			return !this.excluder.isExcluded(element) && super.isIncluded(element);
		}

		@Override
		public boolean isSelected(Element element) {
			return !this.excluder.isExcluded(element) && super.isSelected(element);
		}

	}

}
