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
package io.sarl.eclipse.builder;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.JavaModelManager;

/** Initializer of the classpath container dedicated to the SARL environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLClasspathContainerInitializer extends ClasspathContainerInitializer {
    
	@Override
    public void initialize(IPath containerPath, IJavaProject project)
            throws CoreException {
        IClasspathContainer container = new SARLClasspathContainer(project.getProject());
        JavaCore.setClasspathContainer(containerPath, 
        		new IJavaProject[] {project}, 
        		new IClasspathContainer[] {container}, null);
    }

    @Override
    public boolean canUpdateClasspathContainer(IPath containerPath, IJavaProject project) {
        // always ok to return classpath container
        return true;
    }

    @Override
    public void requestClasspathContainerUpdate(IPath containerPath, IJavaProject javaProject,
            IClasspathContainer containerSuggestion) throws CoreException {
        if (containerSuggestion instanceof SARLClasspathContainer) {
            ((SARLClasspathContainer) containerSuggestion).reset();
        }
        IClasspathContainer scc = JavaCore.getClasspathContainer(SARLClasspathContainer.CONTAINER_ID, javaProject);
        if (scc instanceof SARLClasspathContainer) {
            ((SARLClasspathContainer) scc).reset();
        }
    }
    
    /**
     * Refresh all the SARL classpath containers.
     * Should do this if the ~/.groovy/lib directory has changed.
     * 
     * @throws JavaModelException
     */
    public static void updateAllSARLClasspathContainers() throws JavaModelException {
        IJavaProject[] projects = JavaModelManager.getJavaModelManager().getJavaModel().getJavaProjects();
        updateSomeGroovyClasspathContainers(projects);
    }
    
    /**
     * Refresh the SARL classpath container for the given project.
     * 
     * @param project - the project for which the class path container sohuld be updated.
     * @throws JavaModelException
     */
    public static void updateSARLClasspathContainer(IJavaProject project) throws JavaModelException {
        updateSomeGroovyClasspathContainers(new IJavaProject[] { project });
    }
    

    private static void updateSomeGroovyClasspathContainers(IJavaProject[] projects) throws JavaModelException {
        List<IJavaProject> affectedProjects = new ArrayList<>(projects.length);
        List<IClasspathContainer> affectedContainers = new ArrayList<>(projects.length);
        for (IJavaProject project : projects) {
            IClasspathContainer scc = JavaCore.getClasspathContainer(SARLClasspathContainer.CONTAINER_ID, project);
            if (scc instanceof SARLClasspathContainer) {
                ((SARLClasspathContainer) scc).reset();
                affectedProjects.add(project);
                affectedContainers.add(null);
            }
        }
        JavaCore.setClasspathContainer(
        		SARLClasspathContainer.CONTAINER_ID,
        		affectedProjects.toArray(new IJavaProject[affectedProjects.size()]), 
                affectedContainers.toArray(new IClasspathContainer[affectedContainers.size()]),
                new NullProgressMonitor());
    }
    
}
