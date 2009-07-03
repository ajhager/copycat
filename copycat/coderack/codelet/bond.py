class BondBottomUpScout(object):
    def run(self, coderack, slipnet, workspace):
        '''
        Choose an object and a neighbor of that object probabilistically by
        intra string salience. Choose a bond facet probabilistically by
        relevance in the string. Check if there is a bond between the two
        descriptors of this facet. Post a bond strength tester codelet with
        urgency a function of the degree of association of bonds of the bond
        category.
        '''
        from_object = workspace.choose_object('instra_string_salience')
        to_object = from_object.choose_neighbor()
        if to_object == None:
            return

        facet = from_object.choose_bond_facet(to_object)
        if facet == None:
            return

        from_descriptor = from_object.descriptor(facet)
        to_descriptor = to_object.descriptor(facet)
        if from_descriptor == None or to_descriptor == None:
            return

        category = from_descriptor.bond_category(to_descriptor)
        if category == None:
            return

        workspace.propose_bond(from_object, to_object, facet,
                               from_descriptor, to_descriptor)
