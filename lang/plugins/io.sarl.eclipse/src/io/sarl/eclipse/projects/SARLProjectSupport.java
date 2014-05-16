/**
 * 
 */
package io.sarl.eclipse.projects;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;

/**
 * Creation of the SARL project structure
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectSupport {

	private static final String XTEXT_NATURE_ID = "org.eclipse.xtext.ui.shared.xtextNature"; //$NON-NLS-1$

	private static final String SOURCE_FOLDER_NAME = "src"; //$NON-NLS-1$
	private static final String MAIN_FOLDER_NAME = "main"; //$NON-NLS-1$
	private static final String JAVA_FOLDER_NAME = "java"; //$NON-NLS-1$
	private static final String SARL_FOLDER_NAME = "sarl"; //$NON-NLS-1$
	private static final String GENERATED_SOURCE_FOLDER_NAME = "generated-sources"; //$NON-NLS-1$
	private static final String XTEND_FOLDER_NAME = "xtend"; //$NON-NLS-1$	

	/**
	 * The different directories composing a SARL project
	 */
	public static final String[] PROJECT_STRUCTURE_PATH = { SOURCE_FOLDER_NAME, 
			SOURCE_FOLDER_NAME + File.pathSeparator + MAIN_FOLDER_NAME,
			SOURCE_FOLDER_NAME + File.pathSeparator + MAIN_FOLDER_NAME + File.pathSeparator + JAVA_FOLDER_NAME,
			SOURCE_FOLDER_NAME + File.pathSeparator + MAIN_FOLDER_NAME + File.pathSeparator + SARL_FOLDER_NAME,
			SOURCE_FOLDER_NAME + File.pathSeparator + MAIN_FOLDER_NAME + File.pathSeparator + GENERATED_SOURCE_FOLDER_NAME,
			SOURCE_FOLDER_NAME + File.pathSeparator + MAIN_FOLDER_NAME + File.pathSeparator + GENERATED_SOURCE_FOLDER_NAME + File.pathSeparator + XTEND_FOLDER_NAME};

	/**
	 * For this marvelous project we need to: - create the default Eclipse project - add the custom project nature - create the folder structure
	 * 
	 * @param projectName
	 * @param location
	 * @param useDefaultLocation
	 * @param natureId
	 * @return the newly created project
	 */
	public static IProject createProject(String projectName, URI location, boolean useDefaultLocation) {
		Assert.isNotNull(projectName);
		Assert.isTrue(projectName.trim().length() > 0);

		IProject project = createBaseProject(projectName, location, useDefaultLocation);
		try {
			addNature(project);
			// TODO the project directory is still not created
			addToProjectStructure(project, PROJECT_STRUCTURE_PATH);
		} catch (CoreException e) {
			e.printStackTrace();
			project = null;
		}

		return project;
	}

	/**
	 * Just do the basics: create a basic project.
	 * 
	 * @param location
	 * @param projectName
	 * @param useDefaultLocation
	 * @return the newly created project
	 */
	private static IProject createBaseProject(String projectName, URI location, boolean useDefaultLocation) {
		assert (location != null);
		// it is acceptable to use the ResourcesPlugin class
		//IProject newProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		IJavaProject newProject = JavaCore.create(ResourcesPlugin.getWorkspace().getRoot().getProject(projectName));

		if (!newProject.exists()) {

			// URI projectLocation = URI.create(location.toString() + File.pathSeparator + projectName);
			URI projectLocation;
			if (useDefaultLocation) {
				projectLocation = location;
			} else {
				projectLocation = URI.create(location.toString() + File.pathSeparator + projectName);
			}
			IProjectDescription desc = newProject.getProject().getWorkspace().newProjectDescription(newProject.getProject().getName());
			desc.setLocationURI(projectLocation);

			IProgressMonitor progressMonitor = new NullProgressMonitor();
			try {
				newProject.create(desc, progressMonitor);
				if (!newProject.isOpen()) {
					newProject.open(progressMonitor);
				}
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}

		return newProject;
	}

	private static void createFolder(IFolder folder) throws CoreException {
		IContainer parent = folder.getParent();
		if (parent instanceof IFolder) {
			createFolder((IFolder) parent);
		}
		if (!folder.exists()) {
			folder.create(false, true, null);
		}
	}

	/**
	 * Create a folder structure with a parent root, overlay, and a few child folders.
	 * 
	 * @param newProject
	 * @param paths
	 * @throws CoreException
	 */
	private static void addToProjectStructure(IProject newProject, String[] paths) throws CoreException {
		for (String path : paths) {
			IFolder etcFolders = newProject.getFolder(path);
			createFolder(etcFolders);
		}
	}

	private static void addNature(IProject project) throws CoreException {

		/*
		 * if (!project.hasNature(io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID)) { IProjectDescription description = project.getDescription(); String[] prevNatures = description.getNatureIds(); String[] newNatures = new String[prevNatures.length + 1]; System.arraycopy(prevNatures, 0, newNatures, 0, prevNatures.length); newNatures[prevNatures.length] = io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID; IProjectNatureDescriptor descriptor = ResourcesPlugin.getWorkspace().getNatureDescriptor(io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID); final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures); description.setNatureIds(newNatures);
		 * 
		 * IProgressMonitor monitor = new NullProgressMonitor(); project.setDescription(description, monitor); }
		 */

		final IProjectDescription description = project.getDescription();
		final List<String> natures = new ArrayList<>(Arrays.asList(description.getNatureIds()));
		natures.add(0, io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID);
		natures.add(1, XTEXT_NATURE_ID);
		//natures.add(2, JavaCore.NATURE_ID); not necessary since the project is already a java project

		final String[] newNatures = natures.toArray(new String[natures.size()]);
		final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);

		// check the status and decide what to do
		if (status.getCode() == IStatus.OK) {
			description.setNatureIds(newNatures);
			IProgressMonitor monitor = new NullProgressMonitor();
			project.setDescription(description, monitor);
		} else {
			// TODO raise a user error
		}

		
		addAdditionalSourceFolder((IJavaProject)project);
	}

	private static void addAdditionalSourceFolder(IJavaProject project) throws JavaModelException {
		IFolder sourceFolder = project.getProject().getFolder(SOURCE_FOLDER_NAME);
		// sourceFolder.create(false, true, null);
		IPackageFragmentRoot root = project.getPackageFragmentRoot(sourceFolder);
		IClasspathEntry[] oldEntries = project.getRawClasspath();
		IClasspathEntry[] newEntries = new IClasspathEntry[oldEntries.length + 1];
		System.arraycopy(oldEntries, 0, newEntries, 0, oldEntries.length);
		newEntries[oldEntries.length] = JavaCore.newSourceEntry(root.getPath());
		project.setRawClasspath(newEntries, null);

	}

}