/**
 * 
 */
package io.sarl.eclipse.navigator.node;

import io.sarl.eclipse.internal.EclipseSARLActivator;
import io.sarl.eclipse.navigator.ISARLProjectElement;
import io.sarl.eclipse.navigator.SARLProjectNavigator;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * Provides the project parent of a SARL custom project to enable a custom view in {@link SARLProjectNavigator}
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectParent implements ISARLProjectElement {
	
	private IProject project;

	/**
	 * Image associated to the SARL Project parent folder
	 */
	private Image image;
	

	private ISARLProjectElement[] children;

	public SARLProjectParent(IProject iProject) {
		this.project = iProject;
	}



	public Image getImage() {
		if (this.image == null) {
			this.image = EclipseSARLActivator.getImage("icons/sarl-project-folder-16.png"); //$NON-NLS-1$
		}
		return this.image;
	}

    private ISARLProjectElement[] initializeChildren(IProject project) {
    	ISARLProjectElement[] ichildren = {
                new SARLProjectSARLNode(this),
                new SARLProjectJAVANode(this)
        };
 
        return ichildren;
    }
    
    
	/* (non-Javadoc)
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getChildren()
	 */
	public ISARLProjectElement[] getChildren() {
        if (this.children == null) {
        	this.children = initializeChildren(this.project);
        }
        // else we have already initialized them
 
        return this.children;
	}
	
	public String getProjectName() {
		return this.project.getName();
	}

	/* (non-Javadoc)
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getText()
	 */
	public String getText() {
		return this.project.getName();
	}

	/* (non-Javadoc)
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#hasChildren()
	 */
	public boolean hasChildren() {
        if (this.children == null) {
            this.children = initializeChildren(this.project);
        }
        // else we have already initialized them
        return this.children.length > 0;
	}

	/* (non-Javadoc)
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getProject()
	 */
	public IProject getProject() {		
		return this.project;
	}

	/* (non-Javadoc)
	 * @see io.sarl.eclipse.navigator.ISARLProjectElement#getParent()
	 */
	public Object getParent() {
		return null;
	}
}
