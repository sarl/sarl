/**
 * 
 */
package io.sarl.eclipse.wizards.newfile;

import io.sarl.eclipse.images.EclipseSARLImages;

import java.io.InputStream;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.xtext.util.StringInputStream;

/**
 * First page of the SARL new file wizard.
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class WizardNewSARLFileCreationPage extends WizardNewFileCreationPage {

	private static final String WIZARD_NAME = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_TITLE = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_DESCRIPTION = "Create a SARL File"; //$NON-NLS-1$
	private static final String SARL_FILE_EXTENSION = "sarl"; //$NON-NLS-1$
	
	/**
	 * 
	 * @param selection
	 */
    public WizardNewSARLFileCreationPage(IStructuredSelection selection) {
        super(WIZARD_NAME, selection);
 
        setTitle(PAGE_TITLE);
        setDescription(PAGE_DESCRIPTION);
        setFileExtension(SARL_FILE_EXTENSION);
        setImageDescriptor(EclipseSARLImages.getImageDescriptor(
        		EclipseSARLImages.NEW_FILE_WIZARD_DIALOG_IMAGE));
    }
    
    private static IPath determinePackageName(IPath path) {
    	if (path != null) {
	    	IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(path.segment(0));
			try {
				if (project != null && project.hasNature(JavaCore.NATURE_ID)) {
					IJavaProject javaProject = JavaCore.create(project);
					for(IClasspathEntry entry : javaProject.getRawClasspath()) {
						if (entry.getPath().isPrefixOf(path)) {
							return path.removeFirstSegments(entry.getPath().segmentCount());
						}
					}
				}
			} catch(Exception e) {
				// Ignore the exceptions since they are not useful (hopefully)
			}
    	}
		return null;
    }
    
    @Override
    protected InputStream getInitialContents() {
    	StringBuilder content = new StringBuilder();
    	
    	IPath folderInWorkspace = getContainerFullPath();
    	
    	IPath packagePath = determinePackageName(folderInWorkspace);
    	if (packagePath != null && packagePath.segmentCount() > 0) {
    		content.append("package "); //$NON-NLS-1$
    		content.append(packagePath.segment(0));
    		for (int i=1; i<packagePath.segmentCount(); ++i) {
        		content.append('.');
        		content.append(packagePath.segment(i));
    		}
    		content.append("\n"); //$NON-NLS-1$
    	}
    	
    	return new StringInputStream(content.toString());
    }	
    	
}
