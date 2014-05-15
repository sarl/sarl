/**
 * 
 */
package io.sarl.eclipse.navigator;

import io.sarl.eclipse.navigator.node.SARLProjectParent;

import java.util.List;
import java.util.Vector;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * SARL custom project navigator content provider
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ContentProvider implements ITreeContentProvider {
    
	private static final Object[] NO_CHILDREN = {};
    
    private ISARLProjectElement[] sarlProjectParents;

    /*
     * (non-Javadoc)
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.
     * Object)
     */
    @Override
    public Object[] getChildren(Object parentElement) {
        Object[] children = null;
        if (SARLProjectWorkbenchRoot.class.isInstance(parentElement)) {
            if (this.sarlProjectParents == null) {
            	this.sarlProjectParents = initializeParent(parentElement);
            }

            children = this.sarlProjectParents;
        } else if (ISARLProjectElement.class.isInstance(parentElement)) {
            children = ((ISARLProjectElement) parentElement).getChildren();
        } else {
            children = NO_CHILDREN;
        }

        return children;
    }

    /*
     * (non-Javadoc)
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object
     * )
     */
    @Override
    public Object getParent(Object element) {
        System.out.println("ContentProvider.getParent: " + element.getClass().getName()); //$NON-NLS-1$
        Object parent = null;
        if (ISARLProjectElement.class.isInstance(element)) {
            parent = ((ISARLProjectElement)element).getParent();
        }
        return parent;
    }

    /*
     * (non-Javadoc)
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.
     * Object)
     */
    @Override
    public boolean hasChildren(Object element) {
        boolean hasChildren = false;

        if (SARLProjectWorkbenchRoot.class.isInstance(element)) {
            hasChildren = this.sarlProjectParents.length > 0;
        } else if (ISARLProjectElement.class.isInstance(element)) {
            hasChildren = ((ISARLProjectElement)element).hasChildren();
        }
        // else it is not one of these so return false
        
        return hasChildren;
    }

    /*
     * (non-Javadoc)
     * @see
     * org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java
     * .lang.Object)
     */
    @Override
    public Object[] getElements(Object inputElement) {
        // This is the same as getChildren() so we will call that instead
        return getChildren(inputElement);
    }

    /*
     * (non-Javadoc)
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        System.out.println("ContentProvider.dispose"); //$NON-NLS-1$
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * @see
     * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface
     * .viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        if (oldInput != null && newInput != null){
        	System.out.println("ContentProvider.inputChanged: old: " + oldInput.getClass().getName() + " new: " + newInput.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
        } else {
        	System.out.println("ContentProvider.inputChanged");
        }
        // TODO Auto-generated method stub

    }

    private ISARLProjectElement[] initializeParent(Object parentElement) {
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();

        List<SARLProjectParent> list = new Vector<>();
        for (int i = 0; i < projects.length; i++) {
            try {
                if (projects[i].getNature(io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID) != null) {
                    list.add(new SARLProjectParent(projects[i]));
                }
            } catch (CoreException e) {
                // Go to the next IProject
            }
        }

        SARLProjectParent[] result = new SARLProjectParent[list.size()];
        list.toArray(result);

        return result;
    }

}