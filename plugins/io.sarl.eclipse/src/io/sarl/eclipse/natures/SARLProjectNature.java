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
	 * ID of this nature.
	 */
	public static final String NATURE_ID = "io.sarl.eclipse.SARLProjectNature"; //$NON-NLS-1$
	
	private IProject project;
	

	@Override
	public void configure() throws CoreException {
		//
	}

	@Override
	public void deconfigure() throws CoreException {
		//
	}

	@Override
	public IProject getProject() {		
		return this.project;
	}

	@Override
	public void setProject(IProject iproject) {		
		this.project = iproject;
	}

}
