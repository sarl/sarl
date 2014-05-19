/**
 * 
 */
package io.sarl.eclipse.natures;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;

import org.eclipse.core.runtime.CoreException;

/**
 * SARL custom project nature
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectNature implements IProjectNature {
	
	/**
	 * ID of this nature
	 */
	public static final String NATURE_ID = "io.sarl.eclipse.SARLProjectNature"; //$NON-NLS-1$
	
	private IProject project;
	

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	public void configure() throws CoreException {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	public void deconfigure() throws CoreException {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject() {		
		return this.project;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	public void setProject(IProject iproject) {		
		this.project = iproject;
	}

}
