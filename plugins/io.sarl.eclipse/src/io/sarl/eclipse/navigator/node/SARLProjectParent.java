/**
 * 
 */
package io.sarl.eclipse.navigator.node;

import io.sarl.eclipse.images.EclipseSARLImages;
import io.sarl.eclipse.navigator.ISARLProjectElement;
import io.sarl.eclipse.navigator.SARLProjectNavigator;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * Provides the project parent of a SARL custom project to enable a custom 
 * view in {@link SARLProjectNavigator}
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

	/** Construct a parent element in a SARL project.
	 *  
	 * @param iProject - the project.
	 */
	public SARLProjectParent(IProject iProject) {
		this.project = iProject;
	}


    @Override
	public Image getImage() {
		if (this.image == null) {
			this.image = EclipseSARLImages.getImage("icons/sarl-project-folder-16.png"); //$NON-NLS-1$
		}
		return this.image;
	}

    private ISARLProjectElement[] initializeChildren() {
    	ISARLProjectElement[] ichildren = {
                new SARLProjectSARLNode(this),
                new SARLProjectJAVANode(this)
        };
 
        return ichildren;
    }
    
    
    @Override
	public ISARLProjectElement[] getChildren() {
        if (this.children == null) {
        	this.children = initializeChildren(/*this.project*/);
        }
        // else we have already initialized them
 
        return this.children;
	}

    @Override
	public String getText() {
		return this.project.getName();
	}

    @Override
	public boolean hasChildren() {
        if (this.children == null) {
            this.children = initializeChildren(/*this.project*/);
        }
        // else we have already initialized them
        return this.children.length > 0;
	}

    @Override
	public IProject getProject() {		
		return this.project;
	}

    @Override
	public Object getParent() {
		return null;
	}
}
