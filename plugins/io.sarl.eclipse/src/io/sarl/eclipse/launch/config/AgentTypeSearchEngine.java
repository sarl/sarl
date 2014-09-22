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
package io.sarl.eclipse.launch.config;

import io.sarl.eclipse.util.PluginUtil;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * Engine for searching agent types in the classpath.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AgentTypeSearchEngine {

	private static final int TICKS = 100;

	/**
	 * Returns the package fragment root of <code>IJavaElement</code>. If the given
	 * element is already a package fragment root, the element itself is returned.
	 *
	 * @param element - the element to search for.
	 * @return the package fragment root.
	 */
	public static IPackageFragmentRoot getPackageFragmentRoot(IJavaElement element) {
		return (IPackageFragmentRoot) element.getAncestor(IJavaElement.PACKAGE_FRAGMENT_ROOT);
	}

	/**
	 * Searches for all agent types in the given scope.
	 * Valid styles are IJavaElementSearchConstants.CONSIDER_BINARIES and
	 * IJavaElementSearchConstants.CONSIDER_EXTERNAL_JARS
	 *
	 * @param context - the searching context.
	 * @param scope - the searching scope.
	 * @return the found types.
	 * @throws InvocationTargetException when cannot launch the search.
	 * @throws InterruptedException when the search is interrupted.
	 */
	public IType[] searchAgentTypes(IRunnableContext context, final IJavaSearchScope scope)
			throws InvocationTargetException, InterruptedException  {
		final IType[][] res = new IType[1][];
		context.run(true, true, new IRunnableWithProgress() {
			@Override
			public void run(IProgressMonitor pm) throws InvocationTargetException {
				res[0] = searchAgentTypes(pm, scope);
			}
		});
		return res[0];
	}

	/**
	 * Searches for all agent types in the given scope.
	 * Valid styles are IJavaElementSearchConstants.CONSIDER_BINARIES and
	 * IJavaElementSearchConstants.CONSIDER_EXTERNAL_JARS
	 *
	 * @param progressMonitor - the progression monitor.
	 * @param scope - the searching scope.
	 * @return the found types.
	 * @throws InvocationTargetException when cannot launch the search.
	 */
	@SuppressWarnings("static-method")
	public IType[] searchAgentTypes(IProgressMonitor progressMonitor, final IJavaSearchScope scope)
			throws InvocationTargetException  {
		progressMonitor.beginTask(Messages.AgentTypeSearchEngine_0, TICKS);
		SearchPattern pattern = SearchPattern.createPattern("io.sarl.lang.core.Agent", //$NON-NLS-1$
				IJavaSearchConstants.CLASS,
				IJavaSearchConstants.IMPLEMENTORS,
				SearchPattern.R_EXACT_MATCH | SearchPattern.R_CASE_SENSITIVE);
		SearchParticipant[] participants = new SearchParticipant[] {
				SearchEngine.getDefaultSearchParticipant(),
		};
		TypeCollector collector = new TypeCollector();
		IProgressMonitor searchMonitor = new SubProgressMonitor(progressMonitor, TICKS);
		try {
			new SearchEngine().search(pattern, participants, scope, collector, searchMonitor);
		} catch (CoreException ce) {
			PluginUtil.log(ce);
		}
		List<IType> result = collector.getResult();
		return result.toArray(new IType[result.size()]);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TypeCollector extends SearchRequestor {

		private static final int BUFFER_SIZE = 200;

		private List<IType> result;

		/**
		 */
		public TypeCollector() {
			this.result = new ArrayList<>(BUFFER_SIZE);
		}

		public List<IType> getResult() {
			return this.result;
		}

		@Override
		public void acceptSearchMatch(SearchMatch match) throws CoreException {
			Object enclosingElement = match.getElement();
			// defensive code
			if (enclosingElement instanceof IType) {
				IType curr = (IType) enclosingElement;
				this.result.add(curr);
			}
		}
	}

}
