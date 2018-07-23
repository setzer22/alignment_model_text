package edu.upc.modelvsdocument;

import org.jbpt.pm.ProcessModel;
import org.jbpt.pm.bpmn.Task;
import org.jbpt.pm.Activity;
import org.jbpt.pm.Gateway;
import org.jbpt.pm.AndGateway;
import org.jbpt.pm.XorGateway;

import org.jbpt.bp.BehaviouralProfile;
import org.jbpt.bp.construct.BPCreatorUnfolding;

import org.jbpt.petri.PetriNet;
import org.jbpt.petri.NetSystem;
import org.jbpt.pm.structure.ProcessModel2NetSystem;


public class JBPTTest {
    public static void main (String[] args) throws Exception {
        // Create the process graph 
        ProcessModel p = new ProcessModel();

        // Create the tasks 
        Task t1 = new Task("1"); Task t3 = new Task("3"); Task t4 = new Task("4"); Task t8 = new Task("8"); Task t9 = new Task("9");
        // Add tasks to process graph 
        p.addTask(t1); p.addTask(t3); p.addTask(t4); p.addTask(t8); p.addTask(t9);

        // Create gateways 
        Gateway s2 = new XorGateway("2"); 
        Gateway s6 = new AndGateway("6"); 
        Gateway j7 = new AndGateway("7"); 
        Gateway j5 = new XorGateway("5");

        // Add gateways to process graph
        p.addGateway(s2); p.addGateway(s6); p.addGateway(j7); p.addGateway(j5);

        // Add control flow edges 
        p.addControlFlow(t1, s2); p.addControlFlow(s2, t3); p.addControlFlow(s2, s6); p.addControlFlow(s2, j5); p.addControlFlow(t3, t4); p.addControlFlow(t4, j5); p.addControlFlow(s6, j7); p.addControlFlow(s6, t8); p.addControlFlow(t8, j7); p.addControlFlow(j7, j5); p.addControlFlow(j5, t9); 

        System.out.println(p.toString());

        NetSystem net = ProcessModel2NetSystem.transform(p);

        /* * Finally, if n is an object of class Petri net that is bounded, we may use a methods based on * Petri net unfolding. It is way more costly than the aforementioned approaches, but assumes only * boundedness. */ 
        BehaviouralProfile bp3 = BPCreatorUnfolding.getInstance().deriveRelationSet(net); 
        System.out.println(bp3.toString());


    }
}
